using System;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using ZstdSharp.Generator.CodeGenerator;
using ZstdSharp.Generator.CodeGenerator.Reporter;

namespace ZstdSharp.Generator.Modify;

internal class ImproveDecompressSequences(
    ProjectBuilder projectBuilder,
    IReporter reporter,
    IReadOnlyList<RefMethods.RefMethodInfo> refMethods)
{
    private const string ZstdExecSequence = "ZSTD_execSequence";
    private const string ZstdDecodeSequence = "ZSTD_decodeSequence";
    private const string ZstdDecompressSequencesBody = "ZSTD_decompressSequences_body";
    private static readonly SyntaxAnnotation PrologueVariableAnnotation = new("prologueVariable");

    private class MethodInline : CSharpSyntaxRewriter
    {
        protected record ArgToVariableInfo(string? TargetName, TypeSyntax Type, ExpressionSyntax Expression, bool Dereference = false)
        {
            public readonly string? TargetName = TargetName;
            public readonly TypeSyntax Type = Type;
            public readonly bool Dereference = Dereference;
            public readonly ExpressionSyntax Expression = Expression;
        }

        protected readonly MethodDeclarationSyntax Method;
        protected readonly VariableDeclaratorSyntax Variable;
        protected readonly IReporter Reporter;
        protected readonly Dictionary<string, ArgToVariableInfo> ArgToVariables = [];

        protected MethodInline(IReporter reporter, MethodDeclarationSyntax method,
            InvocationExpressionSyntax invocationExpression, VariableDeclaratorSyntax variable)
        {
            Reporter = reporter;
            Method = method;
            Variable = variable;

            // arguments op -> replace, &op -> replace and dereference
            foreach (var (i, p) in Method.ParameterList.Parameters.Select((p, index) => (index, p)))
            {
                var argument = invocationExpression.ArgumentList.Arguments[i];
                var type = p.Type!;
                switch (argument.Expression)
                {
                    case IdentifierNameSyntax identifierName:
                        ArgToVariables.Add(p.Identifier.ToString(),
                            new ArgToVariableInfo(identifierName.ToString(), type, argument.Expression));
                        break;
                    case PrefixUnaryExpressionSyntax prefixUnaryExpression when
                        prefixUnaryExpression.Kind() == SyntaxKind.AddressOfExpression &&
                        prefixUnaryExpression.Operand is IdentifierNameSyntax identifierName2:
                        var dereference = p.Modifiers.All(m => m.ToString() != "ref");
                        ArgToVariables.Add(p.Identifier.ToString(),
                            new ArgToVariableInfo(identifierName2.ToString(), type,
                                dereference ? argument.Expression : identifierName2,
                                Dereference: dereference));
                        break;
                    default:
                        ArgToVariables.Add(p.Identifier.ToString(),
                            new ArgToVariableInfo(null, type, argument.Expression));
                        break;
                }
            }
        }

        public override SyntaxNode? VisitExpressionStatement(ExpressionStatementSyntax node)
        {
            if (node.Expression is AssignmentExpressionSyntax assignmentExpression &&
                assignmentExpression.Kind() == SyntaxKind.SimpleAssignmentExpression &&
                assignmentExpression.Left is IdentifierNameSyntax identifierName)
            {
                var name = identifierName.ToString();
                if (ArgToVariables.TryGetValue(name, out var argToVariable) && argToVariable.TargetName == name)
                {
                    var newName = identifierName + "Inner";
                    ArgToVariables[name] = new ArgToVariableInfo(newName, argToVariable.Type, SyntaxFactory.IdentifierName(newName));

                    return SyntaxFactory.LocalDeclarationStatement(
                        SyntaxFactory.VariableDeclaration(argToVariable.Type,
                            SyntaxFactory.SingletonSeparatedList(SyntaxFactory.VariableDeclarator(newName)
                                .WithInitializer(SyntaxFactory.EqualsValueClause(assignmentExpression.Right))))
                    );
                }
            }

            return base.VisitExpressionStatement(node);
        }

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
        {
            // replace usage of parameter to argument
            if (ArgToVariables.TryGetValue(node.Identifier.ToString(), out var argToVariableInfo))
            {
                return argToVariableInfo.Expression;
            }

            return base.VisitIdentifierName(node);
        }

        public override SyntaxNode? VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
        {
            // *op -> op
            if (node.Kind() == SyntaxKind.PointerIndirectionExpression)
            {
                var innerExpression = TreeHelper.GetInnerExpression(node.Operand);
                if (innerExpression is IdentifierNameSyntax identifierName &&
                    ArgToVariables.TryGetValue(identifierName.Identifier.ToString(), out var toName) && toName.Dereference)
                {
                    return SyntaxFactory.IdentifierName(toName.TargetName!);
                }
            }

            return base.VisitPrefixUnaryExpression(node);
        }

        public BlockSyntax Run()
        {
            return (Visit(Method.Body) as BlockSyntax)!;
        }
    }

    private class ExecSequenceInline : MethodInline
    {
        private const string ReturnLabel = "returnOneSeqSize";
        private readonly string _returnVariableName = "";

        public ExecSequenceInline(IReporter reporter, MethodDeclarationSyntax method,
            InvocationExpressionSyntax invocationExpression, VariableDeclaratorSyntax variable) : base(reporter, method, invocationExpression, variable)
        {
            // return sequenceLength;
            var lastStatement = method.Body?.Statements.LastOrDefault();
            if (lastStatement is ReturnStatementSyntax {Expression: IdentifierNameSyntax identifierName1})
            {
                _returnVariableName = identifierName1.ToString();
            }
        }

        public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            // nuint sequenceLength = ...;
            if (node.Declaration is { Variables.Count: 1 } variableDeclaration)
            {
                var variable = variableDeclaration.Variables.First();
                if (variable.Identifier.ToString() == _returnVariableName)
                {
                    ArgToVariables[_returnVariableName] =
                        new ArgToVariableInfo(Variable.Identifier.ToString(), variableDeclaration.Type, SyntaxFactory.IdentifierName(Variable.Identifier));

                    return SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(Variable.Identifier),
                        variable.Initializer!.Value
                    ));
                }
            }

            return base.VisitLocalDeclarationStatement(node);
        }

        public override SyntaxNode? VisitReturnStatement(ReturnStatementSyntax node)
        {
            // return sequenceLength; -> goto returnLabel;
            if (node.Expression is IdentifierNameSyntax identifierName && identifierName.ToString() == _returnVariableName)
            {
                // last return sequenceLength; -> returnLabel: ;
                if (node.Parent is BlockSyntax {Parent: MethodDeclarationSyntax} blockSyntax &&
                    blockSyntax.Statements.LastOrDefault() == node)
                {
                    return SyntaxFactory.LabeledStatement(ReturnLabel, SyntaxFactory.EmptyStatement());
                }

                // goto returnLabel;
                return SyntaxFactory.GotoStatement(SyntaxKind.GotoStatement, SyntaxFactory.IdentifierName(ReturnLabel));
            }

            // oneSeqSize = ...; goto returnLabel;
            return FoldBlockHelper.CombineStatements(
            [
                SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(Variable.Identifier), (Visit(node.Expression!) as ExpressionSyntax)!)),
                SyntaxFactory.GotoStatement(SyntaxKind.GotoStatement, SyntaxFactory.IdentifierName(ReturnLabel)),
            ]);
        }
    }

    private class DecodeSequenceInline(
        IReporter reporter,
        MethodDeclarationSyntax method,
        InvocationExpressionSyntax invocationExpression,
        VariableDeclaratorSyntax variable)
        : MethodInline(reporter, method, invocationExpression, variable)
    {
        public override SyntaxNode? VisitReturnStatement(ReturnStatementSyntax node)
        {
            // remove last return
            var lastStatement = Method.Body?.Statements.LastOrDefault();
            if (lastStatement == node)
            {
                return null;
            }

            Reporter.Report(DiagnosticLevel.Error, $"Unexpected return in {ZstdDecodeSequence}");
            return base.VisitReturnStatement(node);
        }
    }

    private class StructToLocalModifier : CSharpSyntaxRewriter
    {
        private readonly IReporter _reporter;
        private readonly MethodDeclarationSyntax _method;
        private readonly Dictionary<string, StructToVariableInfo> _structToVariables = [];

        record StructToVariableInfo(string Name, TypeSyntax Type, string Prefix, Dictionary<string, TypeSyntax> Fields,
            bool IsParameter) : Variable(Name, Type, Prefix)
        {
            public readonly Dictionary<string, TypeSyntax> Fields = Fields;
        }

        public record Variable(string Name, TypeSyntax Type, string Prefix);

        public StructToLocalModifier(ProjectBuilder projectBuilder, IReporter reporter, MethodDeclarationSyntax method, 
            IReadOnlyList<Variable> variables)
        {
            _reporter = reporter;
            _method = method;

            foreach (var variable in variables)
            {
                if (projectBuilder.TryGetTypeDeclaration(variable.Type.ToString(), out var typeDeclaration, out _) &&
                    typeDeclaration is StructDeclarationSyntax structDeclaration)
                {
                    _structToVariables.Add(variable.Name, new StructToVariableInfo(
                        variable.Name,
                        variable.Type,
                        variable.Prefix,
                        structDeclaration.Members
                            .OfType<FieldDeclarationSyntax>().SelectMany(f =>
                                f.Declaration.Variables.Select(v => (v, f.Declaration.Type)))
                            .ToDictionary(v => v.v.Identifier.ToString(), v => v.Type),
                        _method.ParameterList.Parameters.Any(p => p.Identifier.ToString() == variable.Name)
                    ));
                }
            }
        }

        public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            // sequence.offset -> sequence_offset
            if (node is {Expression: IdentifierNameSyntax identifierName, Name: IdentifierNameSyntax name} &&
                _structToVariables.TryGetValue(identifierName.Identifier.ToString(), out var toName))
            {
                if (!toName.Fields.ContainsKey(name.ToString()))
                {
                    _reporter.Report(DiagnosticLevel.Error, $"Unknown field {name}");
                }

                return SyntaxFactory.IdentifierName($"{toName.Prefix}_{name}");
            }

            return base.VisitMemberAccessExpression(node);
        }

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
        {
            // sequence => new seq_t { ... }
            if (_structToVariables.TryGetValue(node.Identifier.ToString(), out var structToVariableInfo))
            {
                return SyntaxFactory.ObjectCreationExpression(SyntaxFactory.ParseTypeName(structToVariableInfo.Type.ToString()))
                    .WithInitializer(SyntaxFactory.InitializerExpression(
                        SyntaxKind.ObjectInitializerExpression,
                        SyntaxFactory.SeparatedList(InitEnumerable(structToVariableInfo))
                    ));
            }

            static IEnumerable<ExpressionSyntax> InitEnumerable(StructToVariableInfo variableInfo)
            {
                foreach (var field in variableInfo.Fields)
                {
                    yield return SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(field.Key),
                        SyntaxFactory.IdentifierName($"{variableInfo.Prefix}_{field.Key}"));
                }
            }

            return base.VisitIdentifierName(node);
        }

        public override SyntaxNode? VisitBlock(BlockSyntax node)
        {
            // add prologue
            if (node.Parent is MethodDeclarationSyntax)
            {
                var block = (base.VisitBlock(node) as BlockSyntax)!;
                var prologue = new List<StatementSyntax>();
                foreach (var kvp in _structToVariables)
                {
                    // var sequence_litLength = sequence.litLength;
                    if (kvp.Value.IsParameter)
                    {
                        prologue.AddRange(kvp.Value.Fields.Select(field =>
                            SyntaxFactory.LocalDeclarationStatement(
                                    SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName("var"))
                                        .WithVariables(SyntaxFactory.SingletonSeparatedList(SyntaxFactory
                                            .VariableDeclarator(
                                                SyntaxFactory.Identifier($"{kvp.Value.Prefix}_{field.Key}"))
                                            .WithInitializer(SyntaxFactory.EqualsValueClause(
                                                SyntaxFactory.MemberAccessExpression(
                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                    SyntaxFactory.IdentifierName(kvp.Value.Name),
                                                    SyntaxFactory.IdentifierName(field.Key)))))))
                                .WithAdditionalAnnotations(PrologueVariableAnnotation)));
                    }
                    else
                    {
                        // T sequence_litLength;
                        prologue.AddRange(kvp.Value.Fields.Select(field =>
                            SyntaxFactory.LocalDeclarationStatement(
                                    SyntaxFactory.VariableDeclaration(field.Value)
                                        .WithVariables(SyntaxFactory.SingletonSeparatedList(SyntaxFactory
                                            .VariableDeclarator(
                                                SyntaxFactory.Identifier($"{kvp.Value.Prefix}_{field.Key}")))))
                                .WithAdditionalAnnotations(PrologueVariableAnnotation)));
                    }
                }

                return block.WithStatements([..prologue, ..block.Statements]);
            }

            return base.VisitBlock(node);
        }

        public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            if (node.Declaration.Variables.Count == 1 &&
                _structToVariables.TryGetValue(node.Declaration.Variables[0].Identifier.ToString(),
                    out var structToVariable) && !structToVariable.IsParameter)
            {
                return null;
            }

            return base.VisitLocalDeclarationStatement(node);
        }

        public MethodDeclarationSyntax Run()
        {
            return (FoldBlockHelper.FoldBlocks(Visit(_method)) as MethodDeclarationSyntax)!;
        }
    }

    private class DecompressSequencesModifier(
        ProjectBuilder projectBuilder,
        IReporter reporter,
        IReadOnlyList<RefMethods.RefMethodInfo> refMethods)
        : CSharpSyntaxRewriter
    {
        private readonly HashSet<string> _keepVariableOnStack = ["nbSeq"];

        private bool ExtractMethodInvocation(LocalDeclarationStatementSyntax node, string name, HashSet<int> counts,
            [NotNullWhen(true)] out MethodDeclarationSyntax? method,
            [NotNullWhen(true)] out InvocationExpressionSyntax? invocationExpression,
            [NotNullWhen(true)] out VariableDeclarationSyntax? variableDeclaration,
            [NotNullWhen(true)] out VariableDeclaratorSyntax? variable
        )
        {
            method = default;
            variable = default;
            invocationExpression = default;
            variableDeclaration = default;

            if (node.Declaration is not {Variables.Count: 1} variableDeclarationValue)
            {
                return false;
            }

            var variableValue = variableDeclarationValue.Variables.First();
            if (variableValue.Initializer is not
                {
                    Value: InvocationExpressionSyntax
                    {
                        Expression: IdentifierNameSyntax identifierName
                    } invocationExpressionValue
                } || identifierName.ToString() != name)
            {
                return false;
            }

            if (node.Parent is not BlockSyntax)
            {
                reporter.Report(DiagnosticLevel.Error, "Invalid parent for variable declaration");
                return false;
            }

            if (!counts.Contains(invocationExpressionValue.ArgumentList.Arguments.Count))
            {
                reporter.Report(DiagnosticLevel.Error,
                    $"Invalid arguments count for {name}, expected {string.Join(" or ", counts.Select(x => x.ToString()))} got {invocationExpressionValue.ArgumentList.Arguments.Count}");
                return false;
            }

            if (!projectBuilder.TryGetMethod(name, out _, out method))
            {
                reporter.Report(DiagnosticLevel.Error, $"No method {name}");
                return false;
            }

            variable = variableValue;
            invocationExpression = invocationExpressionValue;
            variableDeclaration = variableDeclarationValue;
            return true;
        }

        public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            // nuint oneSeqSize = ZSTD_execSequence(op, oend, sequence, &litPtr, litEnd, prefixStart, vBase, dictEnd);
            if (ExtractMethodInvocation(node, ZstdExecSequence, [8], out var execMethod, out var execInvocationExpression,
                    out var execVariableDeclaration, out var execVariable))
            {
                // nuint oneSeqSize; {inlined ZSTD_execSequence}
                var block = new ExecSequenceInline(reporter, execMethod, execInvocationExpression, execVariable).Run();
                return FoldBlockHelper.CombineStatements(
                [
                    node.WithDeclaration(
                        execVariableDeclaration.WithVariables(
                            SyntaxFactory.SingletonSeparatedList(execVariable.WithInitializer(null)))),
                    block.RemoveNodes(block.GetAnnotatedNodes(PrologueVariableAnnotation),
                        SyntaxRemoveOptions.KeepDirectives)!,
                ]);
            }

            if (ExtractMethodInvocation(node, ZstdDecodeSequence, [2, 3], out var decodeMethod,
                    out var decodeInvocationExpression,
                    out _, out var decodeVariable))
            {
                // convert decode to ref version
                var refMethod = new RefMethods.RefMethodInfo(projectBuilder, ZstdDecodeSequence, new Dictionary<string, RefMethods.RefMethodParameterInfo?> { { "seqState", RefMethods.RefMethodInfo.EmptyParameterInfo } });

                var newDecodeMethod =
                    new RefMethodModifier(reporter, refMethod, []).Run(decodeMethod);

                // seq.X -> sequence_X
                newDecodeMethod = new StructToLocalModifier(projectBuilder, reporter, newDecodeMethod,
                        [new("seq", newDecodeMethod.ReturnType, "sequence")])
                    .Run();

                // {inlined ZSTD_decodeSequence}
                return FoldBlockHelper.CombineStatements(new DecodeSequenceInline(reporter, newDecodeMethod,
                    decodeInvocationExpression,
                    decodeVariable).Run().Statements);
            }

            return base.VisitLocalDeclarationStatement(node);
        }

        public override SyntaxNode? VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            if (node.Expression is IdentifierNameSyntax identifierName &&
                identifierName.ToString() == ZstdExecSequence)
            {
                reporter.Report(DiagnosticLevel.Error, $"Unexpected call to {ZstdExecSequence}");
            }

            return base.VisitInvocationExpression(node);
        }

        public override SyntaxNode? VisitBlock(BlockSyntax node)
        {
            // add prologue
            if (node.Parent is MethodDeclarationSyntax)
            {
                var block = (base.VisitBlock(node) as BlockSyntax)!;
                return block.WithStatements([
                    .. _keepVariableOnStack.Select(var => SyntaxFactory.ExpressionStatement(
                            SyntaxFactory.InvocationExpression(
                                SyntaxFactory.IdentifierName("System.Threading.Thread.VolatileRead"),
                                SyntaxFactory.ArgumentList(SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.Argument(SyntaxFactory.IdentifierName(var))
                                        .WithRefOrOutKeyword(SyntaxFactory.Token(SyntaxKind.RefKeyword))))))
                        .WithLeadingTrivia(
                            SyntaxFactory.Comment($"// HACK, force {var} to stack (better register usage)"))),
                    ..block.Statements
                ]);
            }

            return base.VisitBlock(node);
        }

        public MethodDeclarationSyntax Run(MethodDeclarationSyntax method)
        {
            method = (FoldBlockHelper.FoldBlocks(Visit(method)) as MethodDeclarationSyntax)!;

            // for (...; ...; nbSeq)
            var mainForStatement = method.DescendantNodes().OfType<ForStatementSyntax>().FirstOrDefault(forStatement =>
                                       forStatement.Incrementors.Any(x => x.DescendantNodes().Any(n =>
                                           n is IdentifierNameSyntax name && name.Identifier.ToString() == "nbSeq"))) ??
                                   throw new Exception($"Cannot find main loop in {method.Identifier}");
            var typeContext = new TypeContext(projectBuilder);
            var bitDStream = SyntaxFactory.ParseTypeName("BIT_DStream_t");
            typeContext.AddStructType(bitDStream);

            IReadOnlyList<ExtractStructModifier.VariableFields> variableFields =
            [
                new("seqState.DStream", bitDStream, [
                    new("bitContainer"),
                    new("bitsConsumed"),
                    new("ptr"),
                    new("start"),
                    new("limitPtr")
                ]),
            ];
            var variableContext = new VariableContext();
            variableContext.UnpackStructs(variableFields);

            method = new ExtractStructModifier(typeContext, variableFields, mainForStatement).Run(method);
            return new RefMethodUsageModifier(refMethods, variableContext).Run(method);
        }
    }

    public void Run()
    {
        if (!projectBuilder.TryGetMethod(ZstdDecompressSequencesBody, out _, out _))
        {
            throw new Exception($"No {ZstdDecompressSequencesBody} method, ImproveDecompressSequences failed");
        }

        /*
         * Improve ZSTD_decompressSequences_body:
         *
         * 1) Improve ZSTD_execSequence:
         *  - expand sequence struct to local variables (better register usage)
         *  - use ref versions of methods
         * 2) Inline ZSTD_execSequence into ZSTD_decompressSequences_body
         * 3) Keep nbSeq on stack (better register usage), release edi register
         * 4) * Use refs instead of pointers
         * 5) Inline ZSTD_decodeSequence and expand seq struct to locals
         */
        // expand sequence struct to locals
        projectBuilder.ModifyMethod(ZstdExecSequence, (_, method) =>
            new StructToLocalModifier(projectBuilder, reporter, method,
                [.. method.ParameterList.Parameters.Where(p => p.Identifier.ToString() == "sequence" && p.Type != null)
                    .Select(p =>
                        new StructToLocalModifier.Variable(p.Identifier.ToString(), p.Type!,
                            p.Identifier.ToString()))]).Run());

        // use refs instead of pointers, inline ZSTD_decodeSequence, expand seq struct to locals, extract DStream from seqState to local var
        projectBuilder.ModifyMethod(ZstdDecompressSequencesBody, (_, method) =>
        {
            method = ProjectModifier.AddSkipInit(method, "seqState");
            return new PointerToRefFixedBuffer().Run(
                new DecompressSequencesModifier(projectBuilder, reporter,
                        refMethods)
                    .Run(method));
        });

        // use ref methods in ZSTD_execSequence
        projectBuilder.ModifyMethod(ZstdExecSequence, (_, method) =>
            new RefMethodUsageModifier(refMethods, new VariableContext()).Run(method));
    }
}

using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using ZstdSharp.Generator.CodeGenerator;
using ZstdSharp.Generator.CodeGenerator.Reporter;

namespace ZstdSharp.Generator.Modify;

internal class ImproveDecompressSequences
{
    private readonly ProjectBuilder _projectBuilder;
    private readonly IReporter _reporter;
    private const string ZstdExecSequence = "ZSTD_execSequence";

    public ImproveDecompressSequences(ProjectBuilder projectBuilder, IReporter reporter)
    {
        _projectBuilder = projectBuilder;
        _reporter = reporter;
    }

    private bool AddRefVersionOfMethod(string methodName, IReadOnlySet<string> parameters)
    {
        if (!_projectBuilder.TryGetMethod(methodName, out var builder, out var method))
        {
            return false;
        }

        var overlapCopyModifier = new PointerToRefMethodModifier(_reporter, parameters);
        var modifiedMethod = (overlapCopyModifier.Visit(method) as MethodDeclarationSyntax)!;
        builder.AddMethodWithName(modifiedMethod, methodName + "_ref");
        return true;
    }

    private class ExecSequenceInline : CSharpSyntaxRewriter
    {
        private readonly MethodDeclarationSyntax _method;
        private readonly VariableDeclaratorSyntax _variable;
        private readonly Dictionary<string, ArgToVariableInfo> _argToVariables = new();
        private const string ReturnLabel = "returnOneSeqSize";
        private readonly string _returnVariableName = "";

        private record ArgToVariableInfo(string TargetName, TypeSyntax Type, bool Dereference = false)
        {
            public readonly string TargetName = TargetName;
            public readonly TypeSyntax Type = Type;
            public readonly bool Dereference = Dereference;
        }

        public ExecSequenceInline(IReporter reporter, MethodDeclarationSyntax method,
            InvocationExpressionSyntax invocationExpression, VariableDeclaratorSyntax variable)
        {
            _method = method;
            _variable = variable;

            // arguments op -> replace, &op -> replace and dereference
            foreach (var (i, p) in _method.ParameterList.Parameters.Select((p, index) => (index, p)))
            {
                var argument = invocationExpression.ArgumentList.Arguments[i];
                var type = p.Type!;
                switch (argument.Expression)
                {
                    case IdentifierNameSyntax identifierName:
                        _argToVariables.Add(p.Identifier.ToString(),
                            new ArgToVariableInfo(identifierName.ToString(), type));
                        break;
                    case PrefixUnaryExpressionSyntax prefixUnaryExpression when
                        prefixUnaryExpression.Kind() == SyntaxKind.AddressOfExpression &&
                        prefixUnaryExpression.Operand is IdentifierNameSyntax identifierName2:
                        _argToVariables.Add(p.Identifier.ToString(),
                            new ArgToVariableInfo(identifierName2.ToString(), type, Dereference: true));
                        break;
                    default:
                        reporter.Report(DiagnosticLevel.Error, $"Unknown argument kind {argument}");
                        break;
                }
            }

            // return sequenceLength;
            var lastStatement = method.Body?.Statements.LastOrDefault();
            if (lastStatement is ReturnStatementSyntax {Expression: IdentifierNameSyntax identifierName1})
            {
                _returnVariableName = identifierName1.ToString();
            }
        }

        public override SyntaxNode? VisitExpressionStatement(ExpressionStatementSyntax node)
        {
            if (node.Expression is AssignmentExpressionSyntax assignmentExpression &&
                assignmentExpression.Kind() == SyntaxKind.SimpleAssignmentExpression && 
                assignmentExpression.Left is IdentifierNameSyntax identifierName)
            {
                var name = identifierName.ToString();
                if (_argToVariables.TryGetValue(name, out var argToVariable) && argToVariable.TargetName == name)
                {
                    var newName = identifierName + "Inner";
                    _argToVariables[name] = new ArgToVariableInfo(newName, argToVariable.Type);

                    return SyntaxFactory.LocalDeclarationStatement(
                        SyntaxFactory.VariableDeclaration(argToVariable.Type,
                            SyntaxFactory.SingletonSeparatedList(SyntaxFactory.VariableDeclarator(newName)
                                .WithInitializer(SyntaxFactory.EqualsValueClause(assignmentExpression.Right))))
                    );
                }
            }

            return base.VisitExpressionStatement(node);
        }

        public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            // nuint sequenceLength = ...;
            if (node.Declaration is { Variables.Count: 1 } variableDeclaration)
            {
                var variable = variableDeclaration.Variables.First();
                if (variable.Identifier.ToString() == _returnVariableName)
                {
                    _argToVariables[_returnVariableName] =
                        new ArgToVariableInfo(_variable.Identifier.ToString(), variableDeclaration.Type);

                    return SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(_variable.Identifier),
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
            return FoldBlockHelper.CombineStatements(new StatementSyntax[]
            {
                SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(_variable.Identifier), (Visit(node.Expression!) as ExpressionSyntax)!)),
                SyntaxFactory.GotoStatement(SyntaxKind.GotoStatement, SyntaxFactory.IdentifierName(ReturnLabel)),
            });
        }

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
        {
            // replace usage of parameter to argument
            if (_argToVariables.TryGetValue(node.Identifier.ToString(), out var argToVariableInfo))
            {
                if (argToVariableInfo.Dereference)
                {
                    return SyntaxFactory.PrefixUnaryExpression(SyntaxKind.AddressOfExpression,
                        SyntaxFactory.IdentifierName(argToVariableInfo.TargetName));
                }

                return SyntaxFactory.IdentifierName(argToVariableInfo.TargetName);
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
                    _argToVariables.TryGetValue(identifierName.Identifier.ToString(), out var toName) && toName.Dereference)
                {
                    return SyntaxFactory.IdentifierName(toName.TargetName);
                }
            }

            return base.VisitPrefixUnaryExpression(node);
        }

        public BlockSyntax Run()
        {
            return (Visit(_method.Body) as BlockSyntax)!;
        }
    }

    private class StructToLocalModifier : CSharpSyntaxRewriter
    {
        private readonly IReporter _reporter;
        private readonly MethodDeclarationSyntax _method;
        private readonly Dictionary<string, StructToVariableInfo> _structToVariables = new();

        record StructToVariableInfo(string Name, List<string> FieldNames)
        {
            public readonly string Name = Name;
            public readonly List<string> FieldNames = FieldNames;
        }

        public StructToLocalModifier(ProjectBuilder projectBuilder, IReporter reporter, MethodDeclarationSyntax method, IReadOnlySet<string> parameters)
        {
            _reporter = reporter;
            _method = method;

            foreach (var p in _method.ParameterList.Parameters)
            {
                if (parameters.Contains(p.Identifier.ToString()))
                {
                    var typeName = p.Type;
                    if (typeName != null &&
                        projectBuilder.TryGetTypeDeclaration(typeName.ToString(), out var typeDeclaration))
                    {
                        if (typeDeclaration is StructDeclarationSyntax structDeclaration)
                        {
                            _structToVariables.Add(p.Identifier.ToString(), new StructToVariableInfo(
                                p.Identifier.ToString(),
                                structDeclaration.Members
                                    .OfType<FieldDeclarationSyntax>().SelectMany(f => f.Declaration.Variables)
                                    .Select(v => v.Identifier.ToString()).ToList()
                            ));
                        }
                    }
                }
            }
        }

        public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            // sequence.offset -> sequence_offset
            if (node.Expression is IdentifierNameSyntax identifierName &&
                node.Name is IdentifierNameSyntax name &&
                _structToVariables.TryGetValue(identifierName.Identifier.ToString(), out var toName))
            {
                if (!toName.FieldNames.Contains(name.ToString()))
                {
                    _reporter.Report(DiagnosticLevel.Error, $"Unknown field {name}");
                }

                return SyntaxFactory.IdentifierName($"{toName.Name}_{name}");
            }

            return base.VisitMemberAccessExpression(node);
        }

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
        {
            // sequence => new seq_t { ... }
            if (_structToVariables.TryGetValue(node.Identifier.ToString(), out var structToVariableInfo))
            {
                return SyntaxFactory.ObjectCreationExpression(SyntaxFactory.ParseTypeName("seq_t"))
                    .WithInitializer(SyntaxFactory.InitializerExpression(
                        SyntaxKind.ObjectInitializerExpression,
                        SyntaxFactory.SeparatedList(InitEnumerable(structToVariableInfo))
                    ));
            }

            IEnumerable<ExpressionSyntax> InitEnumerable(StructToVariableInfo variableInfo)
            {
                foreach (var field in variableInfo.FieldNames)
                {
                    yield return SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(field),
                        SyntaxFactory.IdentifierName($"{variableInfo.Name}_{field}"));
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
                // var sequence_litLength = sequence.litLength;
                foreach (var kvp in _structToVariables)
                {
                    prologue.AddRange(kvp.Value.FieldNames.Select(field =>
                        SyntaxFactory.LocalDeclarationStatement(
                            SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName("var"))
                                .WithVariables(SyntaxFactory.SingletonSeparatedList(SyntaxFactory
                                    .VariableDeclarator(SyntaxFactory.Identifier($"{kvp.Key}_{field}"))
                                    .WithInitializer(SyntaxFactory.EqualsValueClause(
                                        SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                                            SyntaxFactory.IdentifierName(kvp.Value.Name),
                                            SyntaxFactory.IdentifierName(field)))))))));
                }

                return block.WithStatements(new SyntaxList<StatementSyntax>(prologue.Concat(block.Statements)));
            }

            return base.VisitBlock(node);
        }

        public MethodDeclarationSyntax Run()
        {
            return (FoldBlockHelper.FoldBlocks(Visit(_method)) as MethodDeclarationSyntax)!;
        }
    }

    private class PointerToRefUsageModifier : CSharpSyntaxRewriter
    {
        private readonly MethodDeclarationSyntax _method;
        private readonly IReadOnlyDictionary<string, MethodInfo> _methods;

        public record MethodInfo(string Name, IReadOnlyList<int> ParameterIndexes)
        {
            public readonly string Name = Name;
            public readonly IReadOnlyList<int> ParameterIndexes = ParameterIndexes;
        }

        public PointerToRefUsageModifier(MethodDeclarationSyntax method, 
            IEnumerable<MethodInfo> methods)
        {
            _method = method;
            _methods = methods.ToDictionary(k => k.Name, k => k);
        }

        public override SyntaxNode? VisitArgument(ArgumentSyntax node)
        {
            if (node.Expression is PrefixUnaryExpressionSyntax prefixUnary &&
                prefixUnary.Kind() == SyntaxKind.AddressOfExpression &&
                node.Parent is ArgumentListSyntax
                {
                    Parent: InvocationExpressionSyntax
                    {
                        Expression: IdentifierNameSyntax identifierName
                    }
                } argumentList &&
                _methods.TryGetValue(identifierName.ToString(), out var methodInfo))
            {
                var index = argumentList.Arguments.IndexOf(node);
                if (methodInfo.ParameterIndexes.Any(p => p == index))
                {
                    return node
                        .WithExpression((ExpressionSyntax) Visit(prefixUnary.Operand))
                        .WithRefOrOutKeyword(SyntaxFactory.Token(SyntaxKind.RefKeyword));
                }
            }

            return base.VisitArgument(node);
        }

        public MethodDeclarationSyntax Run()
        {
            return (FoldBlockHelper.FoldBlocks(Visit(_method)) as MethodDeclarationSyntax)!;
        }
    }

    private class DecompressSequencesModifier : CSharpSyntaxRewriter
    {
        private readonly IReporter _reporter;
        private readonly MethodDeclarationSyntax _method;
        private readonly ProjectBuilder _projectBuilder;
        private HashSet<string> _keepVariableOnStack;

        public DecompressSequencesModifier(ProjectBuilder projectBuilder, IReporter reporter,
            MethodDeclarationSyntax method)
        {
            _projectBuilder = projectBuilder;
            _reporter = reporter;
            _method = method;
            _keepVariableOnStack = new HashSet<string> {"nbSeq"};
        }

        public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            // nuint oneSeqSize = ZSTD_execSequence(op, oend, sequence, &litPtr, litEnd, prefixStart, vBase, dictEnd);
            if (node.Declaration is {Variables.Count: 1} variableDeclaration)
            {
                var variable = variableDeclaration.Variables.First();

                if (variable.Initializer is
                    {
                        Value: InvocationExpressionSyntax
                        {
                            Expression: IdentifierNameSyntax identifierName
                        } invocationExpression
                    } &&
                    identifierName.ToString() == ZstdExecSequence)
                {
                    if (node.Parent is not BlockSyntax)
                    {
                        _reporter.Report(DiagnosticLevel.Error, "Invalid parent for variable declaration");
                        goto _out;
                    }

                    if (invocationExpression.ArgumentList.Arguments.Count != 8)
                    {
                        _reporter.Report(DiagnosticLevel.Error, $"Invalid arguments count for {ZstdExecSequence}");
                        goto _out;
                    }

                    if (!_projectBuilder.TryGetMethod(ZstdExecSequence, out _, out var execMethod))
                    {
                        _reporter.Report(DiagnosticLevel.Error, $"No method {ZstdExecSequence}");
                        goto _out;
                    }

                    // nuint oneSeqSize; {inlined ZSTD_execSequence}
                    var block = new ExecSequenceInline(_reporter, execMethod, invocationExpression, variable).Run();
                    return FoldBlockHelper.CombineStatements(new StatementSyntax[]
                    {
                        node.WithDeclaration(variableDeclaration.WithVariables(SyntaxFactory.SingletonSeparatedList(variable.WithInitializer(null)))),
                        block
                    });
                }
            }

            _out:
            return base.VisitLocalDeclarationStatement(node);
        }

        public override SyntaxNode? VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            if (node.Expression is IdentifierNameSyntax identifierName &&
                identifierName.ToString() == ZstdExecSequence)
            {
                _reporter.Report(DiagnosticLevel.Error, $"Unexpected call to {ZstdExecSequence}");
            }

            return base.VisitInvocationExpression(node);
        }

        public override SyntaxNode? VisitBlock(BlockSyntax node)
        {
            // add prologue
            if (node.Parent is MethodDeclarationSyntax)
            {
                var block = (base.VisitBlock(node) as BlockSyntax)!;
                var prologue = new List<StatementSyntax>();
                foreach (var var in _keepVariableOnStack)
                {
                    // System.Threading.Thread.VolatileRead(ref nbSeq);
                    prologue.Add(
                        SyntaxFactory.ExpressionStatement(SyntaxFactory.InvocationExpression(
                                SyntaxFactory.IdentifierName("System.Threading.Thread.VolatileRead"),
                                SyntaxFactory.ArgumentList(SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.Argument(SyntaxFactory.IdentifierName(var))
                                        .WithRefOrOutKeyword(SyntaxFactory.Token(SyntaxKind.RefKeyword))))))
                            .WithLeadingTrivia(
                                SyntaxFactory.Comment($"// HACK, force {var} to stack (better register usage)"))
                    );
                }

                return block.WithStatements(new SyntaxList<StatementSyntax>(prologue.Concat(block.Statements)));
            }

            return base.VisitBlock(node);
        }

        public MethodDeclarationSyntax Run()
        {
            return (FoldBlockHelper.FoldBlocks(Visit(_method)) as MethodDeclarationSyntax)!;
        }
    }

    public void Run()
    {
        const string zstdOverlapCopy8 = "ZSTD_overlapCopy8";
        /*
         * Improve ZSTD_decompressSequences_body:
         *
         * 1) Improve ZSTD_execSequence:
         *  - sequence: expand struct to local variables (better register usage)
         *  - add ref version of ZSTD_overlapCopy8, replace &ip -> ref ip, &op -> ref op (better register usage)
         * 2) Inline ZSTD_execSequence into ZSTD_decompressSequences_body
         * 3) Keep nbSeq on stack (better register usage), release edi register
         */
        if (!AddRefVersionOfMethod(zstdOverlapCopy8, new HashSet<string> {"ip", "op"}))
        {
            _reporter.Report(DiagnosticLevel.Error,
                $"Failed to add {zstdOverlapCopy8} ref version, ImproveDecompressSequences failed");
            return;
        }

        _projectBuilder.ModifyMethod(ZstdExecSequence, (_, method) =>
            new PointerToRefUsageModifier(method, new PointerToRefUsageModifier.MethodInfo[]
            {
                new(zstdOverlapCopy8, new[] {0, 1})
            }).Run());

        _projectBuilder.ModifyMethod(ZstdExecSequence, (_, method) =>
            new StructToLocalModifier(_projectBuilder, _reporter, method, new HashSet<string> {"sequence"}).Run());

        _projectBuilder.ModifyMethod("ZSTD_decompressSequences_body", (_, method) =>
            new DecompressSequencesModifier(_projectBuilder, _reporter, method).Run());
    }
}

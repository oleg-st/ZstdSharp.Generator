using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
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
    private readonly IReadOnlyList<RefMethodInfo> _refMethods;
    private const string ZstdExecSequence = "ZSTD_execSequence";
    private const string ZstdDecodeSequence = "ZSTD_decodeSequence";
    private const string ZstdDecompressSequencesBody = "ZSTD_decompressSequences_body";
    private static readonly SyntaxAnnotation PrologueVariableAnnotation = new("prologueVariable");

    public ImproveDecompressSequences(ProjectBuilder projectBuilder, IReporter reporter)
    {
        _projectBuilder = projectBuilder;
        _reporter = reporter;
        _refMethods = new List<RefMethodInfo>
        {
            // bit
            new (_projectBuilder, "BIT_initDStream", new HashSet<string> { "bitD" }),
            new (_projectBuilder, "BIT_lookBits", new HashSet<string> { "bitD" }),
            new (_projectBuilder, "BIT_lookBitsFast", new HashSet<string> { "bitD" }),
            new (_projectBuilder, "BIT_skipBits", new HashSet<string> { "bitD" }),
            new (_projectBuilder, "BIT_readBits", new HashSet<string> { "bitD" }),
            new (_projectBuilder, "BIT_readBitsFast", new HashSet<string> { "bitD" }),
            new (_projectBuilder, "BIT_reloadDStreamFast", new HashSet<string> { "bitD" }),
            new (_projectBuilder, "BIT_reloadDStream", new HashSet<string> { "bitD" }),
            new (_projectBuilder, "BIT_reloadDStream_internal", new HashSet<string> { "bitD" }),
            new (_projectBuilder, "BIT_endOfDStream", new HashSet<string> { "DStream" }),
            // decompress
            new (_projectBuilder, "ZSTD_initFseState", new HashSet<string> { "DStatePtr", "bitD" }),
            new (_projectBuilder, "ZSTD_updateFseStateWithDInfo", new HashSet<string> { "DStatePtr", "bitD" }),
            new (_projectBuilder, "ZSTD_overlapCopy8", new HashSet<string> { "ip", "op" }),
        };
    }

    private record RefMethodInfo
    {
        public readonly MethodDeclarationSyntax? Method;
        public readonly FileBuilder? Builder;
        public readonly string MethodName;
        public readonly IReadOnlySet<string> Parameters;
        public readonly IReadOnlySet<int> ParameterIndexes;

        public RefMethodInfo(ProjectBuilder projectBuilder, string methodName, IReadOnlySet<string> parameters)
        {
            MethodName = methodName;
            Parameters = parameters;
            if (projectBuilder.TryGetMethod(methodName, out Builder, out Method))
            {
                ParameterIndexes = parameters
                    .Select(name => Method.ParameterList.Parameters.IndexOf(p => name == p.Identifier.ToString()))
                    .ToImmutableHashSet();
            }
            else
            {
                ParameterIndexes = ImmutableHashSet<int>.Empty;
            }
        }
    }

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
        protected readonly Dictionary<string, ArgToVariableInfo> ArgToVariables = new();

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
            return FoldBlockHelper.CombineStatements(new StatementSyntax[]
            {
                SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(Variable.Identifier), (Visit(node.Expression!) as ExpressionSyntax)!)),
                SyntaxFactory.GotoStatement(SyntaxKind.GotoStatement, SyntaxFactory.IdentifierName(ReturnLabel)),
            });
        }
    }

    private class DecodeSequenceInline : MethodInline
    {
        public DecodeSequenceInline(IReporter reporter, MethodDeclarationSyntax method,
            InvocationExpressionSyntax invocationExpression, VariableDeclaratorSyntax variable) : base(reporter, method, invocationExpression, variable)
        {
        }

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
        private readonly Dictionary<string, StructToVariableInfo> _structToVariables = new();

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
            if (node.Expression is IdentifierNameSyntax identifierName &&
                node.Name is IdentifierNameSyntax name &&
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

            IEnumerable<ExpressionSyntax> InitEnumerable(StructToVariableInfo variableInfo)
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

                return block.WithStatements(new SyntaxList<StatementSyntax>(prologue.Concat(block.Statements)));
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

    private class PointerToRefUsageModifier : CSharpSyntaxRewriter
    {
        private readonly MethodDeclarationSyntax _method;
        private readonly IReadOnlyDictionary<string, RefMethodInfo> _methods;

        public PointerToRefUsageModifier(MethodDeclarationSyntax method, 
            IEnumerable<RefMethodInfo> methods)
        {
            _method = method;
            _methods = methods.ToDictionary(k => k.MethodName, k => k);
        }

        public override SyntaxNode? VisitArgument(ArgumentSyntax node)
        {
            // &e -> ref e
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
                if (methodInfo.ParameterIndexes.Contains(index))
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
        private readonly HashSet<string> _keepVariableOnStack;
        private readonly IReadOnlyList<RefMethodInfo> _refMethods;
        private readonly bool _inlineDecodeSequence;

        public DecompressSequencesModifier(ProjectBuilder projectBuilder, IReporter reporter,
            MethodDeclarationSyntax method, IReadOnlyList<RefMethodInfo> refMethods, bool inlineDecodeSequence)
        {
            _projectBuilder = projectBuilder;
            _reporter = reporter;
            _method = method;
            _keepVariableOnStack = new HashSet<string> {"nbSeq"};
            _refMethods = refMethods;
            _inlineDecodeSequence = inlineDecodeSequence;
        }

        private bool ExtractMethodInvocation(LocalDeclarationStatementSyntax node, string name, IReadOnlySet<int> counts,
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
                _reporter.Report(DiagnosticLevel.Error, "Invalid parent for variable declaration");
                return false;
            }

            if (!counts.Contains(invocationExpressionValue.ArgumentList.Arguments.Count))
            {
                _reporter.Report(DiagnosticLevel.Error,
                    $"Invalid arguments count for {name}, expected {string.Join(" or ", counts.Select(x => x.ToString()))} got {invocationExpressionValue.ArgumentList.Arguments.Count}");
                return false;
            }

            if (!_projectBuilder.TryGetMethod(name, out _, out method))
            {
                _reporter.Report(DiagnosticLevel.Error, $"No method {name}");
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
            if (ExtractMethodInvocation(node, ZstdExecSequence, new HashSet<int> { 8 }, out var execMethod, out var execInvocationExpression,
                    out var execVariableDeclaration, out var execVariable))
            {
                // nuint oneSeqSize; {inlined ZSTD_execSequence}
                var block = new ExecSequenceInline(_reporter, execMethod, execInvocationExpression, execVariable).Run();
                return FoldBlockHelper.CombineStatements(new StatementSyntax[]
                {
                    node.WithDeclaration(
                        execVariableDeclaration.WithVariables(
                            SyntaxFactory.SingletonSeparatedList(execVariable.WithInitializer(null)))),
                    _inlineDecodeSequence
                        ? block.RemoveNodes(block.GetAnnotatedNodes(PrologueVariableAnnotation),
                            SyntaxRemoveOptions.KeepDirectives)!
                        : block,
                });
            }

            if (_inlineDecodeSequence && ExtractMethodInvocation(node, ZstdDecodeSequence, new HashSet<int> { 2, 3 }, out var decodeMethod,
                    out var decodeInvocationExpression,
                    out _, out var decodeVariable))
            {
                // convert decode to ref version
                var newDecodeMethod =
                    new PointerToRefMethodModifier(_reporter, new HashSet<string> {"seqState"}).Run(decodeMethod);

                // seq.X -> sequence_X
                newDecodeMethod = new StructToLocalModifier(_projectBuilder, _reporter, newDecodeMethod,
                        new List<StructToLocalModifier.Variable> {new("seq", newDecodeMethod.ReturnType, "sequence")})
                    .Run();

                // &s -> ref s
                newDecodeMethod = new PointerToRefUsageModifier(newDecodeMethod, _refMethods).Run();

                // {inlined ZSTD_decodeSequence}
                return FoldBlockHelper.CombineStatements(new DecodeSequenceInline(_reporter, newDecodeMethod,
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
            return new PointerToRefUsageModifier(
                (FoldBlockHelper.FoldBlocks(Visit(_method)) as MethodDeclarationSyntax)!, _refMethods).Run();
        }
    }

    private class ExtractDStream : CSharpSyntaxRewriter
    {
        private readonly ProjectBuilder _projectBuilder;
        private readonly MethodDeclarationSyntax _method;
        private readonly IReporter _reporter;
        private ForStatementSyntax? _mainForStatement;

        private record Field(string Name, string FieldName, TypeSyntax Type);

        private readonly List<Field> _fields;

        public ExtractDStream(ProjectBuilder projectBuilder, IReporter reporter, MethodDeclarationSyntax method)
        {
            _projectBuilder = projectBuilder;
            _reporter = reporter;
            _method = method;
            _fields = new List<Field>();
        }

        public override SyntaxNode? VisitForStatement(ForStatementSyntax node)
        {
            if (node == _mainForStatement)
            {
                return FoldBlockHelper.CombineStatements(
                    _fields.Select(f => SyntaxFactory.LocalDeclarationStatement(
                        SyntaxFactory.VariableDeclaration(f.Type)
                            .WithVariables(
                                SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.VariableDeclarator(
                                            SyntaxFactory.Identifier($"{f.Name}_{f.FieldName}"))
                                        .WithInitializer(
                                            SyntaxFactory.EqualsValueClause(
                                                SyntaxFactory.IdentifierName($"{f.Name}.{f.FieldName}"))))))).Concat(
                        new[]
                        {
                            (base.VisitForStatement(node) as StatementSyntax)!
                        }).ToArray());
            }
        
            return base.VisitForStatement(node);
        }

        public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            // seqState.DStream -> seqState_DStream, below main loop for
            if (_mainForStatement != null &&
                node.SpanStart > _mainForStatement.SpanStart &&
                node.Kind() == SyntaxKind.SimpleMemberAccessExpression &&
                node.Expression is IdentifierNameSyntax identifierName &&
                node.Name is IdentifierNameSyntax name)
            {
                var field = _fields.FirstOrDefault(f =>
                    f.Name == identifierName.Identifier.ToString() && name.Identifier.ToString() == f.FieldName);
                if (field != null)
                {
                    return SyntaxFactory.IdentifierName($"{field.Name}_{field.FieldName}");
                }
            }

            return base.VisitMemberAccessExpression(node);
        }


        public MethodDeclarationSyntax Run()
        {
            foreach (var node in _method.DescendantNodes())
            {
                // for (...; ...; nbSeq)
                if (node is ForStatementSyntax forStatement &&
                    forStatement.Incrementors.Any(x => x.DescendantNodes().Any(n =>
                        n is IdentifierNameSyntax name && name.Identifier.ToString() == "nbSeq")))
                {
                    _mainForStatement = forStatement;
                    break;
                }
            }

            if (_mainForStatement == null)
            {
                _reporter.Report(DiagnosticLevel.Error, $"Cannot find main loop in {_method.Identifier}");
            }

            // find seqState.DStream
            var varName = "seqState";
            var fieldNames = new HashSet<string> {"DStream"};
            var localDeclaration = _method.DescendantNodes().OfType<LocalDeclarationStatementSyntax>()
                .FirstOrDefault(l => l.Declaration.Variables.Any(v => v.Identifier.ToString() == varName));

            if (localDeclaration != null &&
                _projectBuilder.TryGetTypeDeclaration(localDeclaration.Declaration.Type.ToString(),
                    out var typeDeclaration, out _) &&
                typeDeclaration is StructDeclarationSyntax structDeclaration)
            {
                foreach (var field in structDeclaration.Members.OfType<FieldDeclarationSyntax>().SelectMany(f =>
                                 f.Declaration.Variables.Select(v => (v, f.Declaration.Type)))
                             .Where(t => fieldNames.Contains(t.v.Identifier.ToString())))
                {
                    _fields.Add(new Field(varName, field.v.Identifier.ToString(), field.Type));
                }
            }

            if (_fields.Count == 0)
            {
                _reporter.Report(DiagnosticLevel.Error, $"Cannot find {varName} in {_method.Identifier}");
            }

            return (FoldBlockHelper.FoldBlocks(Visit(_method)) as MethodDeclarationSyntax)!;
        }
    }

    public void Run()
    {
        if (!_projectBuilder.TryGetMethod(ZstdDecompressSequencesBody, out var builder, out var decompressMethod))
        {
            _reporter.Report(DiagnosticLevel.Warning,
                $"No {ZstdDecompressSequencesBody} method, ImproveDecompressSequences failed");
            return;
        }

        /*
         * Improve ZSTD_decompressSequences_body:
         *
         * 1) Improve ZSTD_execSequence:
         *  - expand sequence struct to local variables (better register usage)
         *  - add ref version of ZSTD_overlapCopy8, replace &ip -> ref ip, &op -> ref op (better register usage)
         * 2) Inline ZSTD_execSequence into ZSTD_decompressSequences_body
         * 3) Keep nbSeq on stack (better register usage), release edi register
         *
         * For .NET8 additional:
         *
         * Use refs instead of pointers more
         * Inline ZSTD_decodeSequence and expand seq struct to locals
         */
        foreach (var refMethod in _refMethods)
        {
            if (refMethod.Method != null)
            {
                refMethod.Builder!.AddMethodWithName(
                    new PointerToRefMethodModifier(_reporter, refMethod.Parameters)
                        .Run(refMethod.Method),
                    refMethod.MethodName + "_ref");
            }
            else
            {
                _reporter.Report(DiagnosticLevel.Warning, $"Method {refMethod.MethodName} is not found");
            }
        }

        // call ref versions in ZSTD_execSequence
        _projectBuilder.ModifyMethod(ZstdExecSequence, (_, method) =>
            new PointerToRefUsageModifier(method, _refMethods).Run());

        // expand sequence struct to locals
        _projectBuilder.ModifyMethod(ZstdExecSequence, (_, method) =>
            new StructToLocalModifier(_projectBuilder, _reporter, method,
                method.ParameterList.Parameters.Where(p => p.Identifier.ToString() == "sequence" && p.Type != null)
                    .Select(p =>
                        new StructToLocalModifier.Variable(p.Identifier.ToString(), p.Type!,
                            p.Identifier.ToString()))
                    .ToList()).Run());

        // NET8+ version: use refs instead of pointers, inline ZSTD_decodeSequence, expand seq struct to locals, extract DStream from seqState to local var
        _projectBuilder.ModifyMethod(ZstdDecompressSequencesBody, (_, method) =>
            ProjectModifier.WrapWithIfDefined(
                new ExtractDStream(_projectBuilder, _reporter,
                        new PointerToRefFixedBuffer().Run(
                            new DecompressSequencesModifier(_projectBuilder, _reporter,
                                    ProjectModifier.AddSkipInit(method, "seqState"), _refMethods, true)
                                .Run()))
                    .Run(),
                "NET8_0_OR_GREATER"));

        // NET7 and lower version
        builder.AddMethodWithName(
            ProjectModifier.WrapWithIfDefined(
                new DecompressSequencesModifier(_projectBuilder, _reporter,
                    decompressMethod,
                    _refMethods.Where(m => m.MethodName == "ZSTD_overlapCopy8").ToList(), false).Run(),
                "!NET8_0_OR_GREATER"), decompressMethod + "_net7");
    }
}

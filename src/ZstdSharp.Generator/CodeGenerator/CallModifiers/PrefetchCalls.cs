using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;
using ZstdSharp.Generator.Modify;

namespace ZstdSharp.Generator.CodeGenerator.CallModifiers;

internal class PrefetchCalls : ICallsModifier
{
    internal class CallReplacementPrefetch: CallReplacer.CallReplacement
    {
        internal static readonly SyntaxAnnotation PrefetchCallAnnotation = new("prefetchCall");

        private readonly int _level;

        public override SyntaxNode Apply(InvocationExpressionSyntax invocationExpressionSyntax)
        {
            return ProjectModifier.WrapWithIfDefined(SyntaxFactory.IfStatement(
                    SyntaxFactory.IdentifierName("System.Runtime.Intrinsics.X86.Sse.IsSupported"),
                    SyntaxFactory.Block(
                        SyntaxFactory.SingletonList<StatementSyntax>(
                            SyntaxFactory.ExpressionStatement(
                                SyntaxFactory.InvocationExpression(
                                        SyntaxFactory.IdentifierName(
                                            $"System.Runtime.Intrinsics.X86.Sse.Prefetch{_level}"))
                                    .WithArgumentList(invocationExpressionSyntax.ArgumentList))))),
                "NETCOREAPP3_0_OR_GREATER")
                .WithAdditionalAnnotations(PrefetchCallAnnotation);
        }

        public CallReplacementPrefetch(int level) : base(new TypeCaster.TypeCaster.VoidType())
        {
            _level = level;
        }
    }

    private class Reducer : IReducer
    {
        private record PrefetchBlock(IfStatementSyntax IfStatement, BlockSyntax Block)
        {
            public StatementSyntax Root => IfStatement;

            public static PrefetchBlock? Parse(SyntaxNode syntaxNode) =>
                syntaxNode is IfStatementSyntax
                {
                    Statement: BlockSyntax {Statements.Count: 1} block
                } ifStatement &&
                block.Statements[0] is ExpressionStatementSyntax
                {
                    Expression: InvocationExpressionSyntax invocation
                }
                    ? new PrefetchBlock(ifStatement, block)
                    : null;

            public PrefetchBlock Merge(PrefetchBlock nextPrefetchBlock)
            {
                var block = Block.WithStatements(
                    SyntaxFactory.List(
                        Block.Statements.Concat(nextPrefetchBlock.Block.Statements))
                );
                return new PrefetchBlock(IfStatement.WithStatement(block), block);
            }
        }

        [return: NotNullIfNotNull("sourceNode")]
        public T? Reduce<T>(T? sourceNode) where T : SyntaxNode
        {
            if (sourceNode == null)
            {
                return null;
            }

            while (true)
            {
                var annotatedNode = sourceNode.GetAnnotatedNodes(CallReplacementPrefetch.PrefetchCallAnnotation)
                    .OfType<IfStatementSyntax>().FirstOrDefault();
                if (annotatedNode == null)
                {
                    break;
                }

                var prefetchBlock = PrefetchBlock.Parse(annotatedNode);
                if (prefetchBlock != null && annotatedNode.Parent is BlockSyntax parentBlock)
                {
                    var index = parentBlock.Statements.IndexOf(annotatedNode);
                    if (index >= 0 && index + 1 < parentBlock.Statements.Count)
                    {
                        var nextStatement = parentBlock.Statements[index + 1];
                        var nextPrefetchBlock = PrefetchBlock.Parse(nextStatement);
                        if (nextPrefetchBlock != null &&
                            nextPrefetchBlock.Root.HasAnnotation(CallReplacementPrefetch.PrefetchCallAnnotation))
                        {
                            var newPrefetch= prefetchBlock.Merge(nextPrefetchBlock);
                            // replace & delete (null)
                            sourceNode = sourceNode.ReplaceNodes(new[] {prefetchBlock.Root, nextPrefetchBlock.Root},
                                (originalNode, _) => originalNode == prefetchBlock.Root
                                    ? newPrefetch.Root
                                    : null!);
                            continue;
                        }
                    }
                }

                sourceNode = sourceNode.ReplaceNode(annotatedNode,
                    annotatedNode.WithoutAnnotations(CallReplacementPrefetch.PrefetchCallAnnotation));
            }

            return sourceNode;
        }
    }

    public void Init(ProjectBuilder projectBuilder)
    {
        projectBuilder.RegisterReducer(new Reducer());
    }

    public IEnumerable<(string, CallReplacer.CallReplacement)> GetCallReplacements()
    {
        yield return ("Prefetch0", new CallReplacementPrefetch(0));
        yield return ("Prefetch1", new CallReplacementPrefetch(1));
    }

    public IEnumerable<(string, string)> GetMacros()
    {
        yield return ("PREFETCH_L1", "PREFETCH_L1(ptr) Prefetch0(ptr)");
        yield return ("PREFETCH_L2", "PREFETCH_L2(ptr) Prefetch1(ptr)");
    }

    public string GetDefinitions() => "void Prefetch0(const void* ptr);\r\n" +
                                      "void Prefetch1(const void* ptr);";

}
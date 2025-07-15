using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ZstdSharp.Generator.Modify;

internal static class FoldBlockHelper
{
    private static readonly SyntaxAnnotation ExpandBlockAnnotation = new("foldBlock");

    public static SyntaxNode? CombineStatements(IReadOnlyCollection<StatementSyntax> statements)
    {
        if (statements.Count == 0)
        {
            return null;
        }

        return statements.Count == 1
            ? statements.First()
            : SyntaxFactory.Block(statements).WithAdditionalAnnotations(ExpandBlockAnnotation);
    }

    public static TSyntax Wrap<TSyntax>(TSyntax node) where TSyntax : SyntaxNode =>
        node.WithAdditionalAnnotations(ExpandBlockAnnotation);

    public static SyntaxNode FoldBlocks(SyntaxNode sourceNode)
    {
        while (true)
        {
            var node = sourceNode.GetAnnotatedNodes(ExpandBlockAnnotation).FirstOrDefault();
            if (node == null)
                break;

            var expanded = false;
            var parent = node.Parent;
            if (parent is BlockSyntax)
            {
                sourceNode = sourceNode.ReplaceNode(node, node.ChildNodes());
                expanded = true;
            }
            else
            {
                // Block (Label: ExpandBlock (S1, S2, ...)) -> Block (Label: S1, S2, ...)
                // Block (Label: Label: ExpandBlock (S1, S2, ...)) -> Block (Label: Label: S1, S2, ...)
                while (parent is LabeledStatementSyntax)
                {
                    var nextParent = parent.Parent;
                    if (nextParent is BlockSyntax)
                    {
                        var firstChild = node.ChildNodes().First();
                        var otherChildren = node.ChildNodes().Skip(1).ToList();
                        if (otherChildren.Count > 0)
                        {
                            sourceNode = sourceNode.InsertNodesAfter(node.Parent!, otherChildren);
                            node = sourceNode.GetAnnotatedNodes(ExpandBlockAnnotation)
                                .FirstOrDefault()!;
                        }

                        sourceNode = sourceNode.ReplaceNode(node, firstChild);
                        expanded = true;
                        break;
                    }

                    parent = nextParent;
                }
            }

            if (!expanded)
            {
                sourceNode = sourceNode.ReplaceNode(node,
                    node.WithoutAnnotations(ExpandBlockAnnotation));
            }
        }

        return sourceNode;
    }
}

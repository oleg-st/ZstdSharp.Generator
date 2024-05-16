using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using ZstdSharp.Generator.CodeGenerator;

namespace ZstdSharp.Generator.Modify;

internal class StructInitialization
{
    private bool IsNodesSame(SyntaxNode left, SyntaxNode right)
    {
        if (left is ExpressionSyntax leftExpression)
        {
            left = TreeHelper.GetInnerExpression(leftExpression);
        }

        if (right is ExpressionSyntax rightExpression)
        {
            right = TreeHelper.GetInnerExpression(rightExpression);
        }

        // todo check two expressions is the same (x.y = x.y)
        return left.GetType() == right.GetType() && left.ToString() == right.ToString();
    }

    private bool HasSameNodesInside(SyntaxNode parent, IEnumerable<SyntaxNode> needle)
        => parent.DescendantNodesAndSelf().Any(x => needle.Any(n => IsNodesSame(x, n)));

    public MethodDeclarationSyntax Process(MethodDeclarationSyntax method)
    {
    start:
        foreach (var node in method.DescendantNodes())
        {
            // x = new T();
            if (node is ObjectCreationExpressionSyntax
                {
                    Parent: AssignmentExpressionSyntax assignmentExpression
                } objectCreationExpression &&
                assignmentExpression.Kind() == SyntaxKind.SimpleAssignmentExpression &&
                assignmentExpression.Parent is ExpressionStatementSyntax
                {
                    Parent: BlockSyntax parentBlock
                } expressionStatement)
            {
                var index = parentBlock.Statements.IndexOf(expressionStatement);
                // not found ?
                if (index < 0)
                {
                    continue;
                }

                var nodes = new List<SyntaxNode>();
                var initExpressions = new List<ExpressionSyntax>();
                if (objectCreationExpression.Initializer != null)
                {
                    initExpressions.AddRange(objectCreationExpression.Initializer.Expressions);
                }

                for (index++; index < parentBlock.Statements.Count; index++)
                {
                    var nextStatement = parentBlock.Statements[index];
                    // skip assert
                    if (nextStatement is ExpressionStatementSyntax
                        {
                            Expression: InvocationExpressionSyntax { Expression: IdentifierNameSyntax identifierName }
                        } && identifierName.Identifier.ToString() == "assert")
                    {
                        // go next statement
                        continue;
                    }

                    if (nextStatement is ExpressionStatementSyntax
                        {
                            Expression: AssignmentExpressionSyntax
                            nextAssignmentExpressionSyntax
                        } && nextAssignmentExpressionSyntax.Kind() ==
                        SyntaxKind.SimpleAssignmentExpression)
                    {
                        var leftExpression = TreeHelper.GetInnerExpression(assignmentExpression.Left);
                        if (
                            nextAssignmentExpressionSyntax
                                .Left is MemberAccessExpressionSyntax memberAccessExpression &&
                            (
                                // *x = new T(); x->f = ...
                                (
                                    memberAccessExpression.Kind() == SyntaxKind.PointerMemberAccessExpression &&
                                    leftExpression is PrefixUnaryExpressionSyntax prefixUnaryExpression &&
                                    prefixUnaryExpression.Kind() == SyntaxKind.PointerIndirectionExpression &&
                                    IsNodesSame(memberAccessExpression.Expression, prefixUnaryExpression.Operand)
                                ) ||
                                // x = new T(); x.f = ...
                                (
                                    memberAccessExpression.Kind() == SyntaxKind.SimpleMemberAccessExpression &&
                                    IsNodesSame(memberAccessExpression.Expression, leftExpression)
                                )
                            ) &&
                            // check init expressions inside expression
                            !HasSameNodesInside(nextAssignmentExpressionSyntax.Right, initExpressions
                                .OfType<AssignmentExpressionSyntax>()
                                .Where(x => x.Left is IdentifierNameSyntax)
                                .Select(x =>
                                    SyntaxFactory.MemberAccessExpression(memberAccessExpression.Kind(),
                                        memberAccessExpression.Expression, (x.Left as IdentifierNameSyntax)!)))
                        )
                        {
                            nodes.Add(nextStatement);
                            initExpressions.Add(SyntaxFactory.AssignmentExpression(
                                SyntaxKind.SimpleAssignmentExpression,
                                memberAccessExpression.Name, nextAssignmentExpressionSyntax.Right));
                            // go next statement
                            continue;
                        }
                    }

                    break;
                }

                if (nodes.Count > 0)
                {
                    // replace and start over
                    var newObjectCreationExpression = objectCreationExpression
                        .WithInitializer(SyntaxFactory.InitializerExpression(
                            SyntaxKind.ObjectInitializerExpression, SyntaxFactory.SeparatedList(initExpressions)))
                        .WithArgumentList(
                            objectCreationExpression.ArgumentList is { Arguments.Count: > 0 }
                                ? objectCreationExpression.ArgumentList
                                : null);

                    method = method.ReplaceNodes(nodes.Concat(new[] { objectCreationExpression }),
                        (originalNode, _) => originalNode == objectCreationExpression
                            ? newObjectCreationExpression
                            : null!);
                    goto start;
                }
            }
        }

        return method;
    }
}

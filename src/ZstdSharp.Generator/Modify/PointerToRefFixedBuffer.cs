using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using ZstdSharp.Generator.CodeGenerator;

namespace ZstdSharp.Generator.Modify;

internal class PointerToRefFixedBuffer: CSharpSyntaxRewriter
{
    public override SyntaxNode? VisitElementAccessExpression(ElementAccessExpressionSyntax node)
    {
        var expression = TreeHelper.GetInnerExpression(node.Expression);
        if (expression is PrefixUnaryExpressionSyntax prefixUnaryExpression &&
            node.ArgumentList.Arguments.Count == 1 &&
            prefixUnaryExpression.Kind() == SyntaxKind.AddressOfExpression &&
            prefixUnaryExpression.Operand is MemberAccessExpressionSyntax memberAccess &&
            memberAccess.Kind() == SyntaxKind.SimpleMemberAccessExpression &&
            memberAccess.Name.Identifier.ToString() == "e0")
        {
            // (&f.e0)[i] -> System.Runtime.CompilerServices.Unsafe.Add(ref f.e0, (int)i)
            return SyntaxFactory.InvocationExpression(
                SyntaxFactory.IdentifierName("System.Runtime.CompilerServices.Unsafe.Add"),
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList(
                        new List<ArgumentSyntax>
                        {
                            SyntaxFactory.Argument(memberAccess)
                                .WithRefKindKeyword(SyntaxFactory.Token(SyntaxKind.RefKeyword)),
                            SyntaxFactory.Argument(SyntaxFactory.CastExpression(SyntaxFactory.ParseTypeName("int"),
                                node.ArgumentList.Arguments[0].Expression))
                        }
                    )
                )
            );
        }

        return base.VisitElementAccessExpression(node);
    }

    [return: NotNullIfNotNull("node")]
    public T? Run<T>(T? node) where T : SyntaxNode
    {
        return Visit(node) as T;
    }
}

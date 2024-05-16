using ClangSharp;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

namespace ZstdSharp.Generator.CodeGenerator.CallModifiers;

internal class CallReplacementMemset : CallReplacer.CallReplacementInvocation
{
    private static SizeOfExpressionSyntax? GetSizeOfExpression(ExpressionSyntax expressionSyntax)
    {
        if (expressionSyntax is ParenthesizedExpressionSyntax parenthesizedExpression)
        {
            return GetSizeOfExpression(parenthesizedExpression.Expression);
        }

        if (expressionSyntax is CastExpressionSyntax castExpression)
        {
            return GetSizeOfExpression(castExpression.Expression);
        }

        if (expressionSyntax is SizeOfExpressionSyntax sizeOfExpression)
        {
            return sizeOfExpression;
        }

        return null;
    }

    private static Expr GetDestExpr(Expr expr)
    {
        if (expr is ImplicitCastExpr implicitCastExpr)
        {
            return GetDestExpr(implicitCastExpr.SubExpr);
        }

        if (expr is ParenExpr parenExpr)
        {
            return GetDestExpr(parenExpr.SubExpr);
        }

        return expr;
    }

    public override SyntaxNode Apply(InvocationExpressionSyntax invocationExpressionSyntax, Expr expr,
        CodeGenerator codeGenerator)
    {
        // memset(dest, ch, count)
        if (invocationExpressionSyntax.ArgumentList.Arguments.Count == 3 && expr is CallExpr {NumArgs: 3} callExpr)
        {
            // count -> sizeof(T), ch -> 0
            var sizeOfExpression =
                GetSizeOfExpression(invocationExpressionSyntax.ArgumentList.Arguments[2].Expression);
            if (sizeOfExpression != null && TreeHelper.GetValueOfType<int>(invocationExpressionSyntax.ArgumentList.Arguments[1]
                    .Expression) == 0)
            {
                var destExpr = GetDestExpr(callExpr.Args[0]);
                var destType = codeGenerator.GetRemappedCSharpType(destExpr, destExpr.Type, out _);
                // dest -> T*
                if (destType is PointerTypeSyntax pointerType &&
                    pointerType.ElementType.ToString() == sizeOfExpression.Type.ToString())
                {
                    var destExpression =
                        TreeHelper.GetInnerExpression(invocationExpressionSyntax.ArgumentList.Arguments[0]
                            .Expression);
                    // &x -> x or x -> *x
                    var expressionSyntax = SyntaxFactory.ParenthesizedExpression(
                        destExpression is PrefixUnaryExpressionSyntax prefixUnaryExpression &&
                        prefixUnaryExpression.Kind() == SyntaxKind.AddressOfExpression
                            ? prefixUnaryExpression.Operand
                            : SyntaxFactory.PrefixUnaryExpression(SyntaxKind.PointerIndirectionExpression,
                                destExpression)
                    );

                    // *dest = new T();
                    return SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        expressionSyntax,
                        SyntaxFactory.ObjectCreationExpression(sizeOfExpression.Type)
                            .WithArgumentList(SyntaxFactory.ArgumentList())
                    );
                }
            }
        }

        return invocationExpressionSyntax;
    }

    public CallReplacementMemset() : base("memset", new TypeCaster.TypeCaster.VoidType(),
        "static ZstdSharp.UnsafeHelper", new TypeCaster.TypeCaster.CustomType[]
        {
            new TypeCaster.TypeCaster.PointerType("void*"), TypeCaster.TypeCaster.IntegerType.Create("byte"),
            TypeCaster.TypeCaster.IntegerType.Create("uint")
        })
    {
    }
}

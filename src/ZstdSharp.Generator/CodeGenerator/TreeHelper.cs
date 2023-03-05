using System.Diagnostics.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ZstdSharp.Generator.CodeGenerator;

internal static class TreeHelper
{
    public static bool IsEmptyStatement([NotNullWhen(false)] StatementSyntax? statementSyntax)
        => statementSyntax is null or EmptyStatementSyntax or BlockSyntax { Statements.Count: 0 };

    public static ExpressionSyntax GetInnerExpression(ExpressionSyntax expressionSyntax)
    {
        if (expressionSyntax is ParenthesizedExpressionSyntax parenthesizedExpression)
        {
            return GetInnerExpression(parenthesizedExpression.Expression);
        }

        return expressionSyntax;
    }
}

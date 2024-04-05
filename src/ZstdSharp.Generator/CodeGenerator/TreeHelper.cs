using System.Diagnostics.CodeAnalysis;
using System.Security.Cryptography.Pkcs;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
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

    public static T? GetValueOfType<T>(SyntaxNode node) where T : struct => 
        GetValue(node, out var v) && v is T typeValue ? typeValue : null;

    public static bool GetValue(SyntaxNode node, out object? value)
    {
        value = default;

        if (node is LiteralExpressionSyntax literalExpression)
        {
            if (literalExpression.Kind() == SyntaxKind.TrueLiteralExpression)
            {
                value = true;
                return true;
            }

            if (literalExpression.Kind() == SyntaxKind.FalseLiteralExpression)
            {
                value = false;
                return true;
            }

            if (literalExpression.Kind() == SyntaxKind.NumericLiteralExpression && literalExpression.Token.Value is int v)
            {
                value = v;
                return true;

            }
        }

        if (node is ParenthesizedExpressionSyntax parenthesizedExpression)
        {
            return GetValue(parenthesizedExpression.Expression, out value);
        }

        if (node is BinaryExpressionSyntax binaryExpression &&
            GetValue(binaryExpression.Left, out var leftValue) &&
            GetValue(binaryExpression.Right, out var rightValue))
        {
            if (leftValue is int leftIntValue && rightValue is int rightIntValue)
            {
                switch (binaryExpression.Kind())
                {
                    case SyntaxKind.AddExpression:
                        value = leftIntValue + rightIntValue;
                        return true;
                    case SyntaxKind.SubtractExpression:
                        value = leftIntValue - rightIntValue;
                        return true;
                    case SyntaxKind.MultiplyExpression:
                        value = leftIntValue * rightIntValue;
                        return true;
                    case SyntaxKind.DivideExpression:
                        value = leftIntValue / rightIntValue;
                        return true;
                    case SyntaxKind.EqualsExpression:
                        value = leftIntValue == rightIntValue;
                        return true;
                    case SyntaxKind.LessThanExpression:
                        value = leftIntValue < rightIntValue;
                        return true;
                    case SyntaxKind.GreaterThanExpression:
                        value = leftIntValue > rightIntValue;
                        return true;
                    case SyntaxKind.LessThanOrEqualExpression:
                        value = leftIntValue <= rightIntValue;
                        return true;
                    case SyntaxKind.GreaterThanOrEqualExpression:
                        value = leftIntValue >= rightIntValue;
                        return true;
                    case SyntaxKind.NotEqualsExpression:
                        value = leftIntValue != rightIntValue;
                        return true;
                    // todo other
                }
            }

            if (leftValue is bool leftBoolValue && rightValue is bool rightBoolValue)
            {
                switch (binaryExpression.Kind())
                {
                    case SyntaxKind.LogicalAndExpression:
                        value = leftBoolValue && rightBoolValue;
                        return true;
                    case SyntaxKind.LogicalOrExpression:
                        value = leftBoolValue || rightBoolValue;
                        return true;
                }
            }
        }

        if (node is PrefixUnaryExpressionSyntax prefixUnaryExpression &&
            GetValue(prefixUnaryExpression.Operand, out var opValue))
        {
            if (opValue is bool opBoolValue)
            {
                switch (prefixUnaryExpression.Kind())
                {
                    case SyntaxKind.LogicalNotExpression:
                        return !opBoolValue;
                }
            }
        }

        value = default;
        return false;
    }
}

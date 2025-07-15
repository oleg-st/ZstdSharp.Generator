using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
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

    public static T? GetValueOfType<T>(SyntaxNode node, Dictionary<string, object?>? values = null) where T : struct => 
        GetValue(node, out var v, values) && v is T typeValue ? typeValue : null;

    public static bool GetValue(SyntaxNode node, out object? value, Dictionary<string, object?>? values = null)
    {
        value = default;

        if (values != null && node is IdentifierNameSyntax identifierName && 
            values.TryGetValue(identifierName.Identifier.ToString(), out value))
        {
            return true;
        }

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
            return GetValue(parenthesizedExpression.Expression, out value, values);
        }

        if (node is BinaryExpressionSyntax binaryExpression &&
            GetValue(binaryExpression.Left, out var leftValue, values) &&
            GetValue(binaryExpression.Right, out var rightValue, values))
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
                    case SyntaxKind.LeftShiftExpression:
                        value = leftIntValue << rightIntValue;
                        return true;
                    case SyntaxKind.RightShiftExpression:
                        value = leftIntValue >> rightIntValue;
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
            GetValue(prefixUnaryExpression.Operand, out var opValue, values))
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

    public static IEnumerable<SyntaxNode> GetParents(SyntaxNode? node)
    {
        var parent = node?.Parent;
        while (parent != null)
        {
            yield return parent;
            parent = parent.Parent;
        }
    }

    public static SeparatedSyntaxList<T> ReplaceItems<T>(Dictionary<T, List<T>> replacements, IEnumerable<T> items, Func<T, SyntaxNode> visit) where T : SyntaxNode
    {
        var newArgumentsList = new List<T>();
        foreach (var arg in items)
        {
            if (replacements.TryGetValue(arg, out var list))
            {
                newArgumentsList.AddRange(list);
            }
            else
            {
                newArgumentsList.Add((T)visit(arg));
            }
        }

        return SyntaxFactory.SeparatedList(newArgumentsList);
    }

    public static StatementSyntax? GetTopStatement(MethodDeclarationSyntax method, SyntaxNode node)
        => GetParents(node).FirstOrDefault(n => n.Parent == method.Body) as StatementSyntax;
}

using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator.Reporter;

namespace ZstdSharp.Generator.Modify;

/**
 * Rewrites HUF_decompress4X1_usingDTable_internal_fast_c_loop and HUF_decompress4X2_usingDTable_internal_fast_c_loop:
 * convert arrays to local variables:
 * op[0] -> op0, ip[...] -> ip..., op[...] -> op...,
 * memcpy(&bits[0], &args->bits[0], sizeof(ulong) * 4); -> bits[0] = args->bits[0]; ...
 * unroll for statements
 */
internal class FastCLoopModifier : CSharpSyntaxRewriter
{
    private readonly IReporter _reporter;
    private readonly string[] _varNames;
    private readonly string[] _artificialFixedBuffers;

    private readonly Dictionary<string, int> _variableValues = new();

    private void SetVariableValue(string name, int value)
    {
        _variableValues[name] = value;
    }

    private bool HasVariable(string name) => _variableValues.ContainsKey(name);

    private void RemoveVariable(string name) => _variableValues.Remove(name);

    public FastCLoopModifier(FastCLoopMethod fastCLoopMethod, IReporter reporter)
    {
        _reporter = reporter;
        _varNames = fastCLoopMethod switch
        {
            FastCLoopMethod.Decompress4X1 => new[] {"bits", "ip", "op"},
            FastCLoopMethod.Decompress4X2 => new[] {"bits", "ip", "op", "oend"},
            _ => throw new ArgumentOutOfRangeException(nameof(fastCLoopMethod), fastCLoopMethod, null),
        };

        _artificialFixedBuffers = new[] {"args->ip", "args->op"};
    }

    public MethodDeclarationSyntax? RewriteMethod(MethodDeclarationSyntax method) =>
        FoldBlockHelper.FoldBlocks(Visit(method)) as MethodDeclarationSyntax;

    private ExpressionSyntax GetInnerExpression(ExpressionSyntax expressionSyntax)
    {
        if (expressionSyntax is ParenthesizedExpressionSyntax parenthesizedExpression)
        {
            return GetInnerExpression(parenthesizedExpression.Expression);
        }

        if (expressionSyntax is CastExpressionSyntax castExpression)
        {
            return GetInnerExpression(castExpression.Expression);
        }

        if (expressionSyntax is PrefixUnaryExpressionSyntax prefixUnaryExpression &&
            prefixUnaryExpression.Kind() == SyntaxKind.AddressOfExpression)
        {
            return GetInnerExpression(prefixUnaryExpression.Operand);
        }

        return expressionSyntax;
    }

    private bool GetElementBase(ExpressionSyntax dest, out int start,
        [MaybeNullWhen(false)] out ExpressionSyntax baseExpr)
    {
        if (dest is ElementAccessExpressionSyntax elementAccessExpression)
        {
            baseExpr = elementAccessExpression.Expression;
            if (elementAccessExpression.ArgumentList.Arguments.Count != 1)
            {
                start = default;
                baseExpr = default;
                return false;
            }

            var argument = elementAccessExpression.ArgumentList.Arguments.First();
            var expression = Visit(argument.Expression);
            if (!GetIntValue(expression, out start))
            {
                start = default;
                baseExpr = default;
                return false;
            }
        }
        else
        {
            baseExpr = dest;
            start = 0;
        }

        return true;
    }

    private ExpressionSyntax CreateElementAccess(ExpressionSyntax expression, int index)
    {
        var elementAccessExpressionSyntax = SyntaxFactory.ElementAccessExpression(
            expression, SyntaxFactory.BracketedArgumentList(
                SyntaxFactory.SingletonSeparatedList(
                    SyntaxFactory.Argument(SyntaxFactory.LiteralExpression(
                        SyntaxKind.NumericLiteralExpression,
                        SyntaxFactory.Literal(index))))));
        if (_artificialFixedBuffers.Contains(expression.ToString()))
        {
            return SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, expression,
                SyntaxFactory.IdentifierName($"e{index}"));
        }

        return ConvertElementAccess(elementAccessExpressionSyntax);
    }

    private ExpressionSyntax ConvertElementAccess(ExpressionSyntax expression)
    {
        if (GetElementBase(expression, out var index, out var baseExpr) &&
            baseExpr is IdentifierNameSyntax identifierName && _varNames.Contains(identifierName.ToString()))
        {
            return SyntaxFactory.IdentifierName($"{identifierName}{index}");
        }

        return expression;
    }

    public override SyntaxNode? VisitExpressionStatement(ExpressionStatementSyntax node)
    {
        if (node.Expression is InvocationExpressionSyntax
            {
                Expression: IdentifierNameSyntax identifierName
            } invocationExpression &&
            identifierName.ToString() == "memcpy" && invocationExpression.ArgumentList.Arguments.Count == 3)
        {
            var destArg = GetInnerExpression(invocationExpression.ArgumentList.Arguments[0].Expression);
            var srcArg = GetInnerExpression(invocationExpression.ArgumentList.Arguments[1].Expression);
            var sizeArg = GetInnerExpression(invocationExpression.ArgumentList.Arguments[2].Expression);
            // size = X * Y
            if (sizeArg is not BinaryExpressionSyntax binaryExpression ||
                binaryExpression.Kind() != SyntaxKind.MultiplyExpression)
            {
                _reporter.Report(DiagnosticLevel.Error, "Unknown size in memcpy");
                goto _out;
            }

            // X or Y is integer literal -> size
            if (!GetIntValue(binaryExpression.Left, out var size) &&
                !GetIntValue(binaryExpression.Right, out size))
            {
                _reporter.Report(DiagnosticLevel.Error, "Unknown size in memcpy");
                goto _out;
            }

            // destBase[destStart], srcBase[srcStart]
            if (!GetElementBase(destArg, out var destStart, out var destBase) ||
                !GetElementBase(srcArg, out var srcStart, out var srcBase))
            {
                _reporter.Report(DiagnosticLevel.Error, "Unknown offset in memcpy");
                goto _out;
            }

            // destBase[destStart] = srcBase[srcStart], destBase[destStart + 1] = srcBase[srcStart + 1], ...
            var statements = new List<ExpressionStatementSyntax>();
            for (var i = 0; i < size; i++)
            {
                var src = CreateElementAccess(srcBase, i + srcStart);
                var dest = CreateElementAccess(destBase, i + destStart);
                statements.Add(SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, dest, src)));
            }

            return FoldBlockHelper.CombineStatements(statements);
        }

        _out:
        return base.VisitExpressionStatement(node);
    }

    public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
    {
        var name = node.Identifier.ToString();
        if (_variableValues.TryGetValue(name, out var value))
        {
            return SyntaxFactory.LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(value));
        }

        return base.VisitIdentifierName(node);
    }

    public override SyntaxNode? VisitForStatement(ForStatementSyntax node)
    {
        // for (;;)
        if (node.Incrementors.Count == 0 && node.Declaration == null && node.Initializers.Count == 0 &&
            node.Condition == null)
        {
            goto _out;
        }

        if (node.Incrementors.Count != 1)
        {
            _reporter.Report(DiagnosticLevel.Error, "Too many or too few incrementors in for");
            goto _out;
        }

        if (node.Initializers.Count != 1)
        {
            _reporter.Report(DiagnosticLevel.Error, "Too many or too few initializers in for");
            goto _out;
        }

        if (node.Declaration != null)
        {
            _reporter.Report(DiagnosticLevel.Error, "Declaration in for");
            goto _out;
        }

        if (node.Condition == null)
        {
            _reporter.Report(DiagnosticLevel.Error, "No condition in for");
            goto _out;
        }

        var initializer = node.Initializers.First();
        var incrementor = node.Incrementors.First();
        var condition = node.Condition;
        // var = startValue
        if (initializer is not AssignmentExpressionSyntax assignmentExpression ||
            assignmentExpression.Kind() != SyntaxKind.SimpleAssignmentExpression
            || assignmentExpression.Left is not IdentifierNameSyntax identifierName
            || !GetIntValue(assignmentExpression.Right, out var startValue))
        {
            _reporter.Report(DiagnosticLevel.Error, "Invalid initializer in for");
            goto _out;
        }

        var variableName = identifierName.Identifier.ToString();
        // ++var or var++
        if ((incrementor is not PrefixUnaryExpressionSyntax prefixUnaryExpression ||
             prefixUnaryExpression.Kind() != SyntaxKind.PreIncrementExpression ||
             prefixUnaryExpression.Operand is not IdentifierNameSyntax identifierName2 ||
             identifierName2.Identifier.ToString() != variableName) &&
            (incrementor is not PostfixUnaryExpressionSyntax postfixUnaryExpression ||
             postfixUnaryExpression.Kind() != SyntaxKind.PostIncrementExpression ||
             postfixUnaryExpression.Operand is not IdentifierNameSyntax identifierName3 ||
             identifierName3.Identifier.ToString() != variableName))
        {
            _reporter.Report(DiagnosticLevel.Error, "Invalid incrementor in for");
            goto _out;
        }

        // var < stopValue or var <= stopValue2
        if (condition is BinaryExpressionSyntax {Left: IdentifierNameSyntax identifierName4} binaryExpression &&
            identifierName4.Identifier.ToString() == variableName &&
            GetIntValue(binaryExpression.Right, out var stopValue))
        {
            // stopValue = stopValue2 + 1
            if (binaryExpression.Kind() == SyntaxKind.LessThanEqualsToken)
            {
                stopValue++;
            }
            else if (binaryExpression.Kind() != SyntaxKind.LessThanExpression)
            {
                _reporter.Report(DiagnosticLevel.Error, "Invalid condition in for");
                goto _out;
            }
        }
        else
        {
            _reporter.Report(DiagnosticLevel.Error, "Invalid condition in for");
            goto _out;
        }

        if (HasVariable(variableName))
        {
            _reporter.Report(DiagnosticLevel.Error, $"Variable {variableName} is already exists");
            goto _out;
        }

        var statements = new List<StatementSyntax>();
        for (var i = startValue; i < stopValue; i++)
        {
            SetVariableValue(variableName, i);
            var syntaxNode = Visit(node.Statement.WithLeadingTrivia(node.Statement.GetLeadingTrivia()));
            if (syntaxNode is StatementSyntax statementSyntax)
            {
                statements.Add(statementSyntax);
            }
        }

        RemoveVariable(variableName);

        return FoldBlockHelper.CombineStatements(statements);

        _out:
        return base.VisitForStatement(node);
    }

    public override SyntaxNode? VisitElementAccessExpression(ElementAccessExpressionSyntax node)
    {
        var convertElementAccess = ConvertElementAccess(node);
        return convertElementAccess is ElementAccessExpressionSyntax elementAccessExpression
            ? base.VisitElementAccessExpression(elementAccessExpression)
            : convertElementAccess;
    }

    public override SyntaxNode? VisitConditionalExpression(ConditionalExpressionSyntax node)
    {
        if (GetBoolValue(Visit(node.Condition), out var value))
        {
            return Visit(value ? node.WhenTrue : node.WhenFalse);
        }

        return base.VisitConditionalExpression(node);
    }

    private static bool GetValue(SyntaxNode node, out object? value)
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
        }

        value = default;
        return false;
    }

    private static bool GetIntValue(SyntaxNode node, out int value)
    {
        if (GetValue(node, out var objValue) && objValue is int intValue)
        {
            value = intValue;
            return true;
        }

        value = default;
        return false;
    }

    private static bool GetBoolValue(SyntaxNode node, out bool value)
    {
        if (GetValue(node, out var objValue) && objValue is bool boolValue)
        {
            value = boolValue;
            return true;
        }

        value = default;
        return false;
    }


    public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
    {
        // T* var = stackalloc T[S] -> T var0, var1, ...
        if (node.Declaration.Variables.Any(v => _varNames.Contains(v.Identifier.ToString())))
        {
            if (node.Declaration.Variables.Count != 1)
            {
                _reporter.Report(DiagnosticLevel.Error, "Multiple variable declaration");
                goto _out;
            }

            var variable = node.Declaration.Variables.First();
            var varName = variable.Identifier.ToString();
            var declarationType = node.Declaration.Type;

            if (declarationType is not PointerTypeSyntax pointerType)
            {
                _reporter.Report(DiagnosticLevel.Error, $"Invalid {varName} type {declarationType}");
                goto _out;
            }

            if (variable.Initializer is not
                {Value: StackAllocArrayCreationExpressionSyntax {Initializer: null} stackAllocArrayCreation})
            {
                _reporter.Report(DiagnosticLevel.Error, $"Invalid {varName} initializer");
                goto _out;
            }

            if (stackAllocArrayCreation.Type is not ArrayTypeSyntax {RankSpecifiers.Count: 1} arrayType ||
                arrayType.RankSpecifiers[0].Sizes.Count != 1)
            {
                _reporter.Report(DiagnosticLevel.Error, $"Invalid {varName} stackalloc");
                goto _out;
            }

            if (!GetIntValue(arrayType.RankSpecifiers[0].Sizes[0], out var size))
            {
                _reporter.Report(DiagnosticLevel.Error, $"Invalid {varName} invalid size");
                goto _out;
            }

            return node.WithDeclaration(
                SyntaxFactory.VariableDeclaration(pointerType.ElementType,
                    SyntaxFactory.SeparatedList(
                        Enumerable.Range(0, size).Select(i =>
                            SyntaxFactory.VariableDeclarator(
                                SyntaxFactory.Identifier($"{varName}{i}"))))));
        }

        _out:
        return base.VisitLocalDeclarationStatement(node);
    }
}

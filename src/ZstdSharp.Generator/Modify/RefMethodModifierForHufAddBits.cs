using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using ZstdSharp.Generator.CodeGenerator;
using ZstdSharp.Generator.CodeGenerator.Reporter;

namespace ZstdSharp.Generator.Modify;

internal class RefMethodModifierForHufAddBits(
    IReporter reporter,
    RefMethods.RefMethodInfo method,
    IEnumerable<RefMethods.RefMethodInfo> methods)
    : RefMethodModifier(reporter, method, methods)
{
    public override SyntaxNode? VisitElementAccessExpression(ElementAccessExpressionSyntax node)
    {
        // (&e0)[idx] -> e0
        if (node.ArgumentList.Arguments.Count == 1 && 
            node.ArgumentList.Arguments[0].Expression is IdentifierNameSyntax identifierName && identifierName.Identifier.ToString() == "idx")
        {
            var expression = base.Visit(node.Expression);
            if (expression is ExpressionSyntax innerExpression)
            {
                innerExpression = TreeHelper.GetInnerExpression(innerExpression);
                if (innerExpression is PrefixUnaryExpressionSyntax prefixUnaryExpression && prefixUnaryExpression.Kind() == SyntaxKind.AddressOfExpression)
                {
                    return prefixUnaryExpression.Operand;
                }
            }
        }

        return base.VisitElementAccessExpression(node);
    }

    public override SyntaxNode? VisitExpressionStatement(ExpressionStatementSyntax node)
    {
        if (node.Expression is InvocationExpressionSyntax invocationExpression)
        {
            // assert(idx <= 1) -> remove if true
            if (invocationExpression.Expression is IdentifierNameSyntax identifierName 
                && identifierName.Identifier.ToString() == "assert" && invocationExpression.ArgumentList.Arguments.Count == 1)
            {
                var argument = invocationExpression.ArgumentList.Arguments[0].Expression;
                var value = TreeHelper.GetValueOfType<bool>(argument, new Dictionary<string, object?> { { "idx", 0 } });
                if (value == true)
                {
                    return null;
                }
            }
        }

        return base.VisitExpressionStatement(node);
    }
}

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using ZstdSharp.Generator.CodeGenerator;
using ZstdSharp.Generator.CodeGenerator.Reporter;

namespace ZstdSharp.Generator.Modify;

internal class RefMethodModifier : CSharpSyntaxRewriter
{
    private readonly IReporter _reporter;
    private readonly RefMethods.RefMethodInfo _method;
    private readonly Dictionary<string, RefMethods.RefMethodInfo> _methods;
    private readonly VariableContext _variableContext;

    public RefMethodModifier(IReporter reporter, RefMethods.RefMethodInfo method, IEnumerable<RefMethods.RefMethodInfo> methods)
    {
        _reporter = reporter;
        _method = method;
        _methods = methods.ToDictionary(k => k.MethodName, k => k);
        _variableContext = _method.GetVariableContext();
    }

    public override SyntaxNode? VisitParameterList(ParameterListSyntax node)
    {
        var replacements = _method.GetParameterListReplacements(node);
        if (replacements != null)
        {
            return node.WithParameters(TreeHelper.ReplaceItems(replacements, node.Parameters, Visit));
        }

        return base.VisitParameterList(node);
    }

    public override SyntaxNode? VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
    {
        // *op -> op
        if (node.Kind() == SyntaxKind.PointerIndirectionExpression)
        {
            var innerExpression = TreeHelper.GetInnerExpression(node.Operand);
            if (innerExpression is IdentifierNameSyntax identifierName && _variableContext.Contains(identifierName.Identifier.ToString()))
            {
                return innerExpression;
            }
        }

        return base.VisitPrefixUnaryExpression(node);
    }

    public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
    {
        // op->x -> op.x or op_x
        if (node.Kind() == SyntaxKind.PointerMemberAccessExpression)
        {
            var innerExpression = TreeHelper.GetInnerExpression(node.Expression);
            if (innerExpression is IdentifierNameSyntax identifierName && _variableContext.ContainsOriginal(identifierName.Identifier.ToString()))
            {
                return _variableContext.Resolve(SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    node.Expression, node.Name));
            }
        } else if (node.Kind() == SyntaxKind.SimpleMemberAccessExpression)
        {
            // op.x -> op_x
            var resolvedNode = _variableContext.Resolve(node);
            if (resolvedNode != node)
            {
                return resolvedNode;
            }
        }

        return base.VisitMemberAccessExpression(node);
    }

    // todo
    public override SyntaxNode? VisitArgumentList(ArgumentListSyntax node)
    {
        if (node is
            {
                Parent: InvocationExpressionSyntax
                {
                    Expression: IdentifierNameSyntax identifierName
                }
            } &&
            _methods.TryGetValue(identifierName.ToString(), out var methodInfo))
        {
            var replacements = methodInfo.GetArgumentListReplacements(node, _variableContext);
            if (replacements != null)
            {
                return node.WithArguments(TreeHelper.ReplaceItems(replacements, node.Arguments, Visit));
            }
        }

        return base.VisitArgumentList(node);
    }

    public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
    {
        if (_method.Parameters.ContainsKey(node.Identifier.ToString()))
        {
            _reporter.Report(DiagnosticLevel.Warning, $"Unexpected argument {node}");
        }

        return base.VisitIdentifierName(node);
    }

    [return: NotNullIfNotNull(nameof(method))]
    public MethodDeclarationSyntax? Run(MethodDeclarationSyntax? method)
    {
        return Visit(method) as MethodDeclarationSyntax;
    }
}

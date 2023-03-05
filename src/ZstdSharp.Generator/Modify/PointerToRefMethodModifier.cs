using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator;
using ZstdSharp.Generator.CodeGenerator.Reporter;

namespace ZstdSharp.Generator.Modify;

internal class PointerToRefMethodModifier : CSharpSyntaxRewriter
{
    private readonly IReporter _reporter;
    private readonly IReadOnlySet<string> _names;

    public PointerToRefMethodModifier(IReporter reporter, IReadOnlySet<string> names)
    {
        _reporter = reporter;
        _names = names;
    }

    public override SyntaxNode? VisitParameter(ParameterSyntax node)
    {
        // arg -> ref arg
        if (_names.Contains(node.Identifier.ToString()))
        {
            var type = node.Type;
            if (type is not PointerTypeSyntax pointerType)
            {
                _reporter.Report(DiagnosticLevel.Error, $"Invalid type for argument {node.Identifier}");
                goto _out;
            }

            return node.WithType(pointerType.ElementType).AddModifiers(SyntaxFactory.Token(SyntaxKind.RefKeyword));
        }

        _out:
        return base.VisitParameter(node);
    }

    public override SyntaxNode? VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
    {
        // *op -> op
        if (node.Kind() == SyntaxKind.PointerIndirectionExpression)
        {
            var innerExpression = TreeHelper.GetInnerExpression(node.Operand);
            if (innerExpression is IdentifierNameSyntax identifierName && _names.Contains(identifierName.Identifier.ToString()))
            {
                return innerExpression;
            }
        }

        return base.VisitPrefixUnaryExpression(node);
    }

    public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
    {
        // op->x -> op.x
        if (node.Kind() == SyntaxKind.PointerMemberAccessExpression)
        {
            var innerExpression = TreeHelper.GetInnerExpression(node.Expression);
            if (innerExpression is IdentifierNameSyntax identifierName && _names.Contains(identifierName.Identifier.ToString()))
            {
                return SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, node.Expression, node.Name);
            }
        }

        return base.VisitMemberAccessExpression(node);
    }

    public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
    {
        if (_names.Contains(node.Identifier.ToString()))
        {
            _reporter.Report(DiagnosticLevel.Warning, $"Unexpected argument {node}");
        }

        return base.VisitIdentifierName(node);
    }
}

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace ZstdSharp.Generator.Modify;

class RestoreStructModifier(
    IReadOnlyList<ExtractStructModifier.VariableFields> variableFields,
    StatementSyntax afterStatement)
    : CSharpSyntaxRewriter
{
    [return: NotNullIfNotNull(nameof(node))]
    public override SyntaxNode? Visit(SyntaxNode? node)
    {
        if (node == afterStatement)
        {
            return FoldBlockHelper.CombineStatements([
                afterStatement,
                ..variableFields.SelectMany(v => v.Fields,
                    (v, f) => SyntaxFactory.ExpressionStatement(
                        SyntaxFactory.AssignmentExpression(
                            SyntaxKind.SimpleAssignmentExpression,
                            SyntaxFactory.IdentifierName($"{v.Path}.{f.Path}"),
                            SyntaxFactory.IdentifierName($"{v.Path}_{f.VariableName}"))))
            ])!;
        }

        return base.Visit(node);
    }

    public MethodDeclarationSyntax Run(MethodDeclarationSyntax method)
    {
        return (FoldBlockHelper.FoldBlocks(Visit(method)) as MethodDeclarationSyntax)!;
    }
}

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace ZstdSharp.Generator.Modify;

class ExtractStructModifier(
    TypeContext typeContext,
    IReadOnlyList<ExtractStructModifier.VariableFields> variableFields,
    StatementSyntax beforeStatement)
    : CSharpSyntaxRewriter
{
    public record VariableFields(string Path, TypeSyntax Type, IReadOnlyList<VariableContext.FieldInfo> Fields)
    {
        public string VariableName => Path.Replace('.', '_');
    }

    [return: NotNullIfNotNull(nameof(node))]
    public override SyntaxNode? Visit(SyntaxNode? node)
    {
        if (node == beforeStatement)
        {
            return FoldBlockHelper.CombineStatements([
                ..variableFields.SelectMany(v => v.Fields,
                    (v, f) => SyntaxFactory.LocalDeclarationStatement(SyntaxFactory
                        .VariableDeclaration(typeContext.GetFieldType(f.SplitPath, v.Type))
                        .WithVariables(SyntaxFactory.SingletonSeparatedList(SyntaxFactory
                            .VariableDeclarator(SyntaxFactory.Identifier($"{v.VariableName}_{f.VariableName}"))
                            .WithInitializer(
                                SyntaxFactory.EqualsValueClause(
                                    SyntaxFactory.IdentifierName($"{v.Path}.{f.Path}"))))))),
                beforeStatement
            ])!;
        }

        return base.Visit(node);
    }

    public MethodDeclarationSyntax Run(MethodDeclarationSyntax method)
    {
        return (FoldBlockHelper.FoldBlocks(Visit(method)) as MethodDeclarationSyntax)!;
    }
}

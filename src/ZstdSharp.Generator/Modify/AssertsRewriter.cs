using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator;

namespace ZstdSharp.Generator.Modify;

/*
 * Add DEBUG conditions to some only assert blocks
 */
internal class AssertsRewriter : CSharpSyntaxRewriter
{
    private readonly FileBuilder _fileBuilder;

    public AssertsRewriter(FileBuilder fileBuilder)
    {
        _fileBuilder = fileBuilder;
    }

    public MethodDeclarationSyntax? RewriteMethod(MethodDeclarationSyntax method) =>
        Visit(method) as MethodDeclarationSyntax;

    private static bool IsNodeAssert(SyntaxNode? syntaxNode) =>
        syntaxNode is ExpressionStatementSyntax
        {
            Expression: InvocationExpressionSyntax
            {
                Expression: IdentifierNameSyntax identifierName
            }
        } && identifierName.ToString() == "assert";

    private static bool IsNodeOnlyAsserts(SyntaxNode? node) =>
        IsNodeAssert(node) ||
        node is BlockSyntax { Statements.Count: > 0 } blockSyntax &&
        blockSyntax.Statements
            .All(statement => TreeHelper.IsEmptyStatement(statement) || IsNodeOnlyAsserts(statement));

    public override SyntaxNode? VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        if (IsNodeOnlyAsserts(node.Body))
        {
            // method() { assert }
            _fileBuilder.AddUsingDirective("System.Diagnostics");
            return node.WithAttributeLists(SyntaxFactory.List(
                node.AttributeLists.Concat(new[]
                {
                    SyntaxFactory.AttributeList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Attribute(
                                    SyntaxFactory.IdentifierName("Conditional"))
                                .WithArgumentList(
                                    SyntaxFactory.AttributeArgumentList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.AttributeArgument(
                                                SyntaxFactory.LiteralExpression(
                                                    SyntaxKind.StringLiteralExpression,
                                                    SyntaxFactory.Literal("DEBUG"))
                                            ))))))
                })));
        }

        return base.VisitMethodDeclaration(node);
    }

    public override SyntaxNode? VisitForStatement(ForStatementSyntax node)
    {
        if (IsNodeOnlyAsserts(node.Statement))
        {
            // for () { assert }
            return ProjectModifier.WrapWithIfDefined(node, "DEBUG");
        }

        return base.VisitForStatement(node);
    }

    public override SyntaxNode? VisitIfStatement(IfStatementSyntax node)
    {
        // if () { assert }
        // if () { assert } else { assert }
        if (CodeGenerator.CodeGenerator.IsPureExpr(node.Condition) && 
            IsNodeOnlyAsserts(node.Statement) &&
            (node.Else == null || IsNodeOnlyAsserts(node.Else)))
        {
            return ProjectModifier.WrapWithIfDefined(node, "DEBUG");
        }

        return base.VisitIfStatement(node);
    }
}

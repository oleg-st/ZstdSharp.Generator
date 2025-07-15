using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Linq;
using ZstdSharp.Generator.CodeGenerator;

namespace ZstdSharp.Generator.Modify;

class RefMethodUsageModifier(IEnumerable<RefMethods.RefMethodInfo> methods, VariableContext variableContext) : CSharpSyntaxRewriter
{
    private readonly Dictionary<string, RefMethods.RefMethodInfo> _methods = methods.ToDictionary(k => k.MethodName, k => k);

    public override SyntaxNode? VisitArgumentList(ArgumentListSyntax node)
    {
        if (node is {Parent: InvocationExpressionSyntax {Expression: IdentifierNameSyntax identifierName}} &&
            _methods.TryGetValue(identifierName.ToString(), out var methodInfo))
        {
            var replacements = methodInfo.GetArgumentListReplacements(node, variableContext, true);
            if (replacements != null)
            {
                return node.WithArguments(TreeHelper.ReplaceItems(replacements, node.Arguments, Visit));
            }
        }

        return base.VisitArgumentList(node);
    }

    public MethodDeclarationSyntax Run(MethodDeclarationSyntax method)
    {
        return (Visit(method) as MethodDeclarationSyntax)!;
    }
}

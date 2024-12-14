using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ZstdSharp.Generator.CodeGenerator.VariableSize;

internal class OverrideSize(TypeSyntax type) : IVariableSizeType
{
    private TypeSyntax Type { get; } = type;

    public ExpressionSyntax GetSizeOf(TypeSyntax type)
    {
        return SyntaxFactory.SizeOfExpression(Type);
    }
}
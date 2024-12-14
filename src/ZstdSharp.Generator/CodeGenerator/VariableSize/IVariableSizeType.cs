using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ZstdSharp.Generator.CodeGenerator.VariableSize;

internal interface IVariableSizeType
{
    public ExpressionSyntax GetSizeOf(TypeSyntax type);
}
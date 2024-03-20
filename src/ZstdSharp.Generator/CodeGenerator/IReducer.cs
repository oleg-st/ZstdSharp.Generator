using Microsoft.CodeAnalysis;
using System.Diagnostics.CodeAnalysis;

namespace ZstdSharp.Generator.CodeGenerator;

interface IReducer
{
    [return: NotNullIfNotNull("sourceNode")]
    T? Reduce<T>(T? sourceNode) where T : SyntaxNode;
}

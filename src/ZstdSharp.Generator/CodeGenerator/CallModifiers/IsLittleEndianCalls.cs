using System.Collections.Generic;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

namespace ZstdSharp.Generator.CodeGenerator.CallModifiers;

internal class IsLittleEndianCalls : ICallsModifier
{
    public IEnumerable<(string, CallReplacer.CallReplacement)> GetCallReplacements()
    {
        yield return ("MEM_isLittleEndian",
            new CallReplacer.CallReplacementExpression("BitConverter.IsLittleEndian",
                new TypeCaster.TypeCaster.BoolType(),
                "System"));
        yield return ("IsLittleEndian", new CallReplacer.CallReplacementExpression("BitConverter.IsLittleEndian",
            new TypeCaster.TypeCaster.BoolType(), "System"));
    }

    public IEnumerable<(string, string)> GetMacros()
    {
        yield return ("XXH_CPU_LITTLE_ENDIAN", "XXH_CPU_LITTLE_ENDIAN IsLittleEndian()");
    }

    public string GetDefinitions() => "int IsLittleEndian();";
}
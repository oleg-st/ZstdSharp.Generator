using System.Collections.Generic;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

namespace ZstdSharp.Generator.CodeGenerator.CallModifiers;

internal class Arm64Calls : ICallsModifier
{
    public IEnumerable<(string, CallReplacer.CallReplacement)> GetCallReplacements()
    {
        yield return ("IsArm64", new CallReplacer.CallReplacementExpression("MEM_arm64", new TypeCaster.TypeCaster.BoolType()));
    }

    public IEnumerable<(string, string)> GetMacros()
    {
        yield return ("HUF_4X2_4WAY", "HUF_4X2_4WAY IsArm64()");
    }

    public string GetDefinitions() => "int IsArm64();";
}

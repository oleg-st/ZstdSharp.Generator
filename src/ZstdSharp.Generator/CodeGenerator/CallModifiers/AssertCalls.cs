using System.Collections.Generic;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

namespace ZstdSharp.Generator.CodeGenerator.CallModifiers;

internal class AssertCalls : ICallsModifier
{
    public IEnumerable<(string, CallReplacer.CallReplacement)> GetCallReplacements()
    {
        yield return ("assert", new CallReplacer.CallReplacementIdentity(new TypeCaster.TypeCaster.VoidType(),
            "static ZstdSharp.UnsafeHelper"));
    }

    public IEnumerable<(string, string)> GetMacros()
    {
        yield return ("assert", "assert(c) assert(c)");
        yield return ("XXH_STATIC_ASSERT", "XXH_STATIC_ASSERT(c) assert(c)");
    }

    public string GetDefinitions() => "void assert(int c);";
}
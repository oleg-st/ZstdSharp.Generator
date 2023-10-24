using System.Collections.Generic;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

namespace ZstdSharp.Generator.CodeGenerator.CallModifiers;

internal class PrefetchCalls : ICallsModifier
{
    public IEnumerable<(string, CallReplacer.CallReplacement)> GetCallReplacements()
    {
        yield return ("Prefetch0", new CallReplacer.CallReplacementIdentity(new TypeCaster.TypeCaster.VoidType(),
            "static ZstdSharp.UnsafeHelper"));
        yield return ("Prefetch1", new CallReplacer.CallReplacementIdentity(new TypeCaster.TypeCaster.VoidType(),
            "static ZstdSharp.UnsafeHelper"));
    }

    public IEnumerable<(string, string)> GetMacros()
    {
        yield return ("PREFETCH_L1", "PREFETCH_L1(ptr) Prefetch0(ptr)");
        yield return ("PREFETCH_L2", "PREFETCH_L2(ptr) Prefetch1(ptr)");
    }

    public string GetDefinitions() => "void Prefetch0(const void* ptr);\r\n" +
                                      "void Prefetch1(const void* ptr);";

}
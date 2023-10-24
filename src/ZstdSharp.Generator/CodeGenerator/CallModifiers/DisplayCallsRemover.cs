using System.Collections.Generic;

namespace ZstdSharp.Generator.CodeGenerator.CallModifiers;

internal class DisplayCallsRemover : ICallsModifier
{
    public IEnumerable<(string, string)> GetMacros()
    {
        yield return ("DISPLAY", "DISPLAY(...) {}");
        yield return ("DISPLAYLEVEL", "DISPLAYLEVEL(...) {}");
        yield return ("DISPLAYUPDATE", "DISPLAYUPDATE(...) {}");
    }
}
using System;
using System.Collections.Generic;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

namespace ZstdSharp.Generator.CodeGenerator.CallModifiers;

internal interface ICallsModifier
{
    IEnumerable<(string, CallReplacer.CallReplacement)> GetCallReplacements()
        => Array.Empty<(string, CallReplacer.CallReplacement)>();

    IEnumerable<(string, string)> GetMacros()
        => Array.Empty<(string, string)>();

    string GetDefinitions() => "";
}
using System.Collections.Generic;

namespace ZstdSharp.Generator.CodeGenerator.Macros;

public sealed class MacroStorage
{
    public class MacroInfo
    {
        public string FileName;
        public uint Line;
        public uint Column;
        public uint Offset;
        public bool IsBuiltinMacro;
        public bool IsFunctionLike;
        public uint OffsetEnd;
        public int TokensCount;
        public string MacroValue;
        public string ConstName;

        public MacroInfo(string fileName, uint line, uint column, uint offset, bool isBuiltinMacro, bool isFunctionLike, uint offsetEnd, int tokensCount, string macroValue, string constName)
        {
            FileName = fileName;
            Line = line;
            Column = column;
            Offset = offset;
            IsBuiltinMacro = isBuiltinMacro;
            IsFunctionLike = isFunctionLike;
            OffsetEnd = offsetEnd;
            TokensCount = tokensCount;
            MacroValue = macroValue;
            ConstName = constName;
        }
    }

    public class MacroExpansionInfo
    {
        public string FileName;
        public uint Line;
        public uint Column;
        public uint Offset;
        public uint OffsetEnd;

        public MacroExpansionInfo(string fileName, uint line, uint column, uint offset, uint offsetEnd)
        {
            FileName = fileName;
            Line = line;
            Column = column;
            Offset = offset;
            OffsetEnd = offsetEnd;
        }
    }

    public Dictionary<string, List<MacroInfo>> Macros { get; } = new();

    public Dictionary<string, List<MacroExpansionInfo>> MacroExpansions { get; } = new();

    public void AddMacroInfo(string name, MacroInfo macroInfo)
    {
        if (!Macros.TryGetValue(name, out var list))
        {
            list = new List<MacroInfo>();
            Macros.Add(name, list);
        }

        list.Add(macroInfo);
    }

    public void AddMacroExpansion(string name, MacroExpansionInfo macroExpansionInfo)
    {
        if (!MacroExpansions.TryGetValue(name, out var list))
        {
            list = MacroExpansions[name] = new List<MacroExpansionInfo>();
        }

        list.Add(macroExpansionInfo);
    }

    public void Import(MacroStorage storage)
    {
        foreach (var kvp in storage.Macros)
        {
            foreach (var macroInfo in kvp.Value)
            {
                AddMacroInfo(kvp.Key, macroInfo);
            }
        }

        foreach (var kvp in storage.MacroExpansions)
        {
            foreach (var macroExpansionInfo in kvp.Value)
            {
                AddMacroExpansion(kvp.Key, macroExpansionInfo);
            }
        }
    }
}

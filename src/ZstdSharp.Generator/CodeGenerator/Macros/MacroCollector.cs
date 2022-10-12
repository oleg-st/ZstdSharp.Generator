using System.IO;
using System.Text;
using ClangSharp;
using ClangSharp.Interop;

namespace ZstdSharp.Generator.CodeGenerator.Macros;

public sealed class MacroCollector
{
    public MacroStorage MacroStorage { get; }

    public MacroCollector()
    {
        MacroStorage = new MacroStorage();
    }

    public void CollectMacros(TranslationUnit translationUnit)
    {
        VisitMacros(translationUnit.TranslationUnitDecl);
    }

    private void VisitMacros(Cursor cursor)
    {
        if (cursor is MacroExpansion macroExpansion)
        {
            VisitMacroExpansion(macroExpansion);
        }
        else if (cursor is MacroDefinitionRecord macroDefinitionRecord)
        {
            VisitMacroDefinitionRecord(macroDefinitionRecord);
        }
        else if (cursor is TranslationUnitDecl translationUnitDecl)
        {
            VisitMacrosTranslationUnitDecl(translationUnitDecl);
        }
    }

    private void VisitMacrosTranslationUnitDecl(TranslationUnitDecl translationUnitDecl)
    {
        foreach (var cursor in translationUnitDecl.CursorChildren)
        {
            VisitMacros(cursor);
        }
    }

    private void VisitMacroDefinitionRecord(MacroDefinitionRecord macroDefinitionRecord)
    {
        macroDefinitionRecord.Location.GetFileLocation(out var file, out var line, out var column, out var offset);
        var filename = file.ToString();
        if (!string.IsNullOrEmpty(filename))
        {
            macroDefinitionRecord.Extent.End.GetFileLocation(out _, out _, out _,
                out var offset2);

            var translationUnitHandle = macroDefinitionRecord.TranslationUnit.Handle;
            var tokens = translationUnitHandle.Tokenize(macroDefinitionRecord.Extent).ToArray();

            var macroName = $"ClangSharpMacro_{macroDefinitionRecord.Spelling}";
            var macroValue = "";

            if (tokens[0].Kind == CXTokenKind.CXToken_Identifier &&
                tokens[0].GetSpelling(translationUnitHandle).CString == macroDefinitionRecord.Spelling &&
                tokens.Length > 1)
            {
                var sourceRangeEnd = tokens[^1].GetExtent(translationUnitHandle).End;
                var sourceRangeStart = tokens[1].GetLocation(translationUnitHandle);
                var sourceRange = CXSourceRange.Create(sourceRangeStart, sourceRangeEnd);
                macroValue = GetSourceRangeContents(translationUnitHandle, sourceRange);
            }

            MacroStorage.AddMacroInfo(macroDefinitionRecord.Name, new MacroStorage.MacroInfo(
                Path.GetFullPath(filename),
                line,
                column,
                offset,
                macroDefinitionRecord.IsBuiltinMacro,
                macroDefinitionRecord.IsFunctionLike,
                offset2,
                tokens.Length,
                macroValue,
                macroName
            ));
        }
    }

    private void VisitMacroExpansion(MacroExpansion cursor)
    {
        cursor.Location.GetFileLocation(out var file, out var line, out var column, out var offset);
        var filename = file.ToString();
        if (!string.IsNullOrEmpty(filename))
        {
            cursor.Extent.End.GetFileLocation(out _, out _, out _, out var offset2);

            MacroStorage.AddMacroExpansion(cursor.Name, new MacroStorage.MacroExpansionInfo(
                Path.GetFullPath(filename),
                line,
                column,
                offset,
                offset2
            ));
        }
    }

    private string GetSourceRangeContents(CXTranslationUnit translationUnit, CXSourceRange sourceRange)
    {
        sourceRange.Start.GetFileLocation(out var startFile, out _, out _, out var startOffset);
        sourceRange.End.GetFileLocation(out var endFile, out _, out _, out var endOffset);

        if (startFile != endFile)
        {
            return string.Empty;
        }

        var fileContents = translationUnit.GetFileContents(startFile, out _);
        fileContents = fileContents.Slice(unchecked((int) startOffset), unchecked((int) (endOffset - startOffset)));

        return Encoding.UTF8.GetString(fileContents);
    }
}

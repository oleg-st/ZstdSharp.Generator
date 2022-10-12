using System;
using ClangSharp;
using ClangSharp.Interop;

namespace ZstdSharp.Generator.CodeGenerator;

internal class Parser : IDisposable
{
    private CXIndex _cxIndex;
    private readonly CXErrorCode _cxErrorCode;
    private CXTranslationUnit _cxTranslationUnit;

    public Parser(string filePath, string[] clangCommandLineArgs, CXTranslationUnit_Flags translationFlags, ReadOnlySpan<CXUnsavedFile> unsavedFiles)
    {
        _cxIndex = CXIndex.Create();
        _cxErrorCode = CXTranslationUnit.TryParse(_cxIndex, filePath,
            clangCommandLineArgs, unsavedFiles,
            translationFlags,
            out _cxTranslationUnit);
    }

    public bool Success => _cxErrorCode == CXErrorCode.CXError_Success;

    public CXErrorCode ErrorCode => _cxErrorCode;

    public TranslationUnit GetTranslationUnit()
        => TranslationUnit.GetOrCreate(_cxTranslationUnit);

    public uint NumDiagnostics => _cxTranslationUnit.NumDiagnostics;

    public CXDiagnostic GetDiagnostic(uint index) => _cxTranslationUnit.GetDiagnostic(index);

    public void Dispose()
    {
        _cxIndex.Dispose();
    }
}

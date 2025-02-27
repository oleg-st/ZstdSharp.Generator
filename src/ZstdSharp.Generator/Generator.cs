﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ClangSharp;
using ClangSharp.Interop;
using Microsoft.CodeAnalysis.CSharp;
using ZstdSharp.Generator.CodeGenerator;
using ZstdSharp.Generator.CodeGenerator.CallModifiers;
using ZstdSharp.Generator.CodeGenerator.Macros;
using ZstdSharp.Generator.CodeGenerator.Reporter;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;
using ZstdSharp.Generator.CodeGenerator.VariableSize;
using ZstdSharp.Generator.Modify;

// ReSharper disable StringLiteralTypo
// ReSharper disable CommentTypo

namespace ZstdSharp.Generator;

public class Generator
{
    private readonly string _inputLocation;
    private readonly string _outputLocation;
    private readonly bool _withMultiThread;
    private readonly string _unsafeOutputLocation;
    private readonly string _sourceLocation;
    private readonly List<string> _fileNames;
    private readonly List<string> _clangCommandLineArgs;
    private readonly ProjectBuilder _projectBuilder;
    private readonly List<ICallsModifier> _callsModifiers;

    public Generator(string inputLocation, string outputLocation, bool withMultiThread = true)
    {
        _inputLocation = inputLocation;
        _outputLocation = outputLocation;
        _withMultiThread = withMultiThread;
        _unsafeOutputLocation = Path.Combine(_outputLocation, "Unsafe");
        _sourceLocation = "Source";

        _fileNames = new List<string>
        {
            // common
            "common/debug.c",
            "common/entropy_common.c",
            "common/error_private.c",
            "common/fse_decompress.c",
            "common/xxhash.c",
            "common/zstd_common.c",
            // compress
            "compress/hist.c",
            "compress/huf_compress.c",
            "compress/fse_compress.c",
            "compress/zstd_compress.c",
            "compress/zstd_compress_literals.c",
            "compress/zstd_compress_sequences.c",
            "compress/zstd_compress_superblock.c",
            "compress/zstd_double_fast.c",
            "compress/zstd_fast.c",
            "compress/zstd_lazy.c",
            "compress/zstd_ldm.c",
            "compress/zstd_opt.c",
            "compress/zstd_preSplit.c",
            // decompress
            "decompress/huf_decompress.c",
            "decompress/zstd_ddict.c",
            "decompress/zstd_decompress.c",
            "decompress/zstd_decompress_block.c",
            // dictBuilder
            "dictBuilder/cover.c",
            "dictBuilder/fastcover.c",
            "dictBuilder/zdict.c",
        };
        _clangCommandLineArgs = new List<string>
        {
            "--language=c",
            "--std=c99",
            // turn off dynamic bmi2
            "-DDYNAMIC_BMI2=0",
            // export symbols
            "-DZSTD_DLL_EXPORT=1",
            "-fparse-all-comments",
            // for assert(void *c)
            "-Wno-int-conversion",
        };

        if (_withMultiThread)
        {
            _fileNames.Add("compress/zstdmt_compress.c");
            _fileNames.Add("common/pool.c");
            _clangCommandLineArgs.Add("-DZSTD_MULTITHREAD");
        }

        var reporter = new Reporter();
        _callsModifiers = new List<ICallsModifier>
            {new DisplayCallsRemover(), new PrefetchCalls(), new AssertCalls(), new IsLittleEndianCalls()};
        if (_withMultiThread)
        {
            _callsModifiers.Add(new SynchronizationCalls());
        }
        _projectBuilder = new ProjectBuilder(GetConfig(), reporter);
        foreach (var callsModifier in _callsModifiers)
        {
            callsModifier.Init(_projectBuilder);
        }

        CheckFiles();
    }

    private void CheckFiles()
    {
        foreach (var filename in _fileNames)
        {
            var path = Path.Combine(_inputLocation, filename);
            if (!File.Exists(path))
                throw new Exception($"File is not found {path}");
        }
    }

    private ProjectBuilderConfig GetConfig()
    {
        var namespaceName = "ZstdSharp.Unsafe";
        var remappedNames = new Dictionary<string, string>
        {
            {"size_t", "nuint"},
            {"ptrdiff_t", "nint"},
            {"POOL_ctx_s", "void"},
            {"__m128i", "Vector128<sbyte>"},
            // renamed in 1.5.7, rename back to keep backward compability
            {"ZSTD_ParamSwitch_e", "ZSTD_paramSwitch_e"},
            {"ZSTD_FrameType_e", "ZSTD_frameType_e" },
            {"ZSTD_FrameHeader", "ZSTD_frameHeader" },
            {"ZSTD_SequenceFormat_e", "ZSTD_sequenceFormat_e" },
        };
        var structToClasses = new HashSet<string>
        {
            "ZSTD_LazyVTable",
        };
        var unnecessarySymbols = new HashSet<string>
        {
            "g_debuglevel",
            // COVER
            "g_refreshRate", "g_time", "g_coverCtx",
            "COVER_map_clear", "COVER_map_init", "COVER_prime4bytes", "COVER_map_hash", "COVER_map_index", "COVER_map_at", "COVER_map_remove", "COVER_map_destroy",
            "COVER_cmp", "COVER_cmp8", "COVER_strict_cmp", "COVER_strict_cmp8", "COVER_lower_bound", "COVER_groupBy", "COVER_group", "COVER_selectSegment", "COVER_checkParameters",
            "COVER_ctx_destroy", "COVER_ctx_init", "COVER_buildDictionary", "ZDICT_trainFromBuffer_cover", "COVER_tryParameters", "ZDICT_optimizeTrainFromBuffer_cover",
            "stableSort",
            // ZDICT
            "g_selectivity_default", "ZDICT_clockSpan", "ZDICT_printHex", "ZDICT_getDictID", "ZDICT_getDictHeaderSize", "ZDICT_NbCommonBytes", "ZDICT_count", "ZDICT_initDictItem", "ZDICT_analyzePos", "isIncluded",
            "ZDICT_tryMerge", "ZDICT_removeDictItem", "ZDICT_insertDictItem", "ZDICT_dictSize", "ZDICT_trainBuffer_legacy", "ZDICT_fillNoise", "ZDICT_trainFromBuffer_unsafe_legacy", "ZDICT_trainFromBuffer_legacy",
            // ZstdLazy
            "ZSTD_rotateRight_U64", "ZSTD_rotateRight_U32", "ZSTD_rotateRight_U16",
            // ErrorPrivate
            "_force_has_format_string",
            // 1.5.3
            "ZSTD_cpuSupportsBmi2", "ZSTD_countTrailingZeros32_fallback", "ZSTD_countLeadingZeros32_fallback", 
            // xxhash memory
            "XXH_read32", "XXH_read64",
            // thread pool
            "POOL_ctx_s", "POOL_job_s",
        };
        var callReplacements = new Dictionary<string, CallReplacer.CallReplacement>
        {
            {"MEM_64bits", new CallReplacer.CallReplacementExpression("MEM_64bits", new TypeCaster.BoolType())},
            {"MEM_32bits", new CallReplacer.CallReplacementExpression("MEM_32bits", new TypeCaster.BoolType())},
            {"ZSTD_cpuid_bmi2", new CallReplacer.CallReplacementExpression("0", TypeCaster.IntegerType.Create("int"))},
            {"__builtin_rotateleft32", new CallReplacer.CallReplacementInvocation("BitOperations.RotateLeft", TypeCaster.IntegerType.Create("uint"),
                "System.Numerics", new TypeCaster.CustomType[] { TypeCaster.IntegerType.Create("uint"), TypeCaster.IntegerType.Create("int") })},
            {"__builtin_rotateleft64", new CallReplacer.CallReplacementInvocation("BitOperations.RotateLeft",TypeCaster.IntegerType.Create("ulong"),
                "System.Numerics", new TypeCaster.CustomType[] { TypeCaster.IntegerType.Create("ulong"), TypeCaster.IntegerType.Create("int") })},
            {"_byteswap_ulong", new CallReplacer.CallReplacementInvocation("BinaryPrimitives.ReverseEndianness",TypeCaster.IntegerType.Create("uint"),
                "System.Buffers.Binary", new TypeCaster.CustomType[] { TypeCaster.IntegerType.Create("uint") })},
            {"_byteswap_uint64", new CallReplacer.CallReplacementInvocation("BinaryPrimitives.ReverseEndianness",TypeCaster.IntegerType.Create("ulong"),
                "System.Buffers.Binary", new TypeCaster.CustomType[] { TypeCaster.IntegerType.Create("ulong") })},
            {"ZSTD_rotateRight_U64", new CallReplacer.CallReplacementInvocation("BitOperations.RotateRight",TypeCaster.IntegerType.Create("ulong"),
                "System.Numerics", new TypeCaster.CustomType[] { TypeCaster.IntegerType.Create("ulong"), TypeCaster.IntegerType.Create("int") })},
            {"ZSTD_rotateRight_U32", new CallReplacer.CallReplacementInvocation("BitOperations.RotateRight",TypeCaster.IntegerType.Create("uint"),
                "System.Numerics", new TypeCaster.CustomType[] { TypeCaster.IntegerType.Create("uint"), TypeCaster.IntegerType.Create("int") })},
            {"ZSTD_rotateRight_U16", new CallReplacer.CallReplacementInvocation("BitOperations.RotateRight",TypeCaster.IntegerType.Create("ushort"),
                "System.Numerics", new TypeCaster.CustomType[] { TypeCaster.IntegerType.Create("ushort"), TypeCaster.IntegerType.Create("int") })},
            {"memset", new CallReplacementMemset()},
            {"memcpy", new CallReplacer.CallReplacementInvocation("memcpy",new TypeCaster.VoidType(),
                "static ZstdSharp.UnsafeHelper", new TypeCaster.CustomType[] { new TypeCaster.PointerType("void*"), new TypeCaster.PointerType("void*"), TypeCaster.IntegerType.Create("uint") })},
            {"ZSTD_cpuSupportsBmi2", new CallReplacer.CallReplacementExpression("0", TypeCaster.IntegerType.Create("int"))},
            // bool
            {"ERR_isError", new CallReplacer.CallReplacementIdentity(new TypeCaster.BoolType())},
            {"HUF_isError", new CallReplacer.CallReplacementIdentity(new TypeCaster.BoolType())},
            {"FSE_isError", new CallReplacer.CallReplacementIdentity(new TypeCaster.BoolType())},
            {"HIST_isError", new CallReplacer.CallReplacementIdentity(new TypeCaster.BoolType())},
            {"ZDICT_isError", new CallReplacer.CallReplacementIdentity(new TypeCaster.BoolType())},
            {"ZSTD_isError", new CallReplacer.CallReplacementIdentity(new TypeCaster.BoolType())},
            // string
            {"ERR_getErrorName", new CallReplacer.CallReplacementIdentity(new TypeCaster.StringType())},
            {"ERR_getErrorString", new CallReplacer.CallReplacementIdentity(new TypeCaster.StringType())},
            {"ZSTD_getErrorName", new CallReplacer.CallReplacementIdentity(new TypeCaster.StringType())},
            {"ZSTD_getErrorString", new CallReplacer.CallReplacementIdentity(new TypeCaster.StringType())},
            {"ZSTD_versionString", new CallReplacer.CallReplacementIdentity(new TypeCaster.StringType())},
            {"ZDICT_getErrorName", new CallReplacer.CallReplacementIdentity(new TypeCaster.StringType())},
            {"FSE_getErrorName", new CallReplacer.CallReplacementIdentity(new TypeCaster.StringType())},
            {"HUF_getErrorName", new CallReplacer.CallReplacementIdentity(new TypeCaster.StringType())},
            {"XXH_memcpy", new CallReplacer.CallReplacementIdentity(new TypeCaster.VoidType())},
            // vector
            {"_mm_set1_epi8", new CallReplacer.CallReplacementInvocation("Vector128.Create", new TypeCaster.Vector128Type("Vector128<sbyte>"), "System.Runtime.Intrinsics")},
            {"_mm_loadu_si128", new CallReplacer.CallReplacementInvocation("Sse2.LoadVector128", new TypeCaster.Vector128Type("Vector128<sbyte>"), "System.Runtime.Intrinsics.X86",
                new TypeCaster.CustomType[] { new TypeCaster.PointerType("sbyte*") })},
            {"_mm_cmpeq_epi8", new CallReplacer.CallReplacementInvocation("Sse2.CompareEqual", new TypeCaster.Vector128Type("Vector128<sbyte>"), "System.Runtime.Intrinsics.X86")},
            {"_mm_movemask_epi8", new CallReplacer.CallReplacementInvocation("Sse2.MoveMask", TypeCaster.IntegerType.Create("int"), "System.Runtime.Intrinsics.X86")},
            // fastcover
            {"clock", new CallReplacer.CallReplacementRemove()},
            // remove
            {"__builtin_unreachable", new CallReplacer.CallReplacementRemove()},
            // xxhash memory
            {"XXH_read32", new CallReplacer.CallReplacementInvocation("MEM_read32", TypeCaster.IntegerType.Create("uint"), argumentTypes: new TypeCaster.CustomType[] { new TypeCaster.PointerType("void*") })},
            {"XXH_read64", new CallReplacer.CallReplacementInvocation("MEM_read64", TypeCaster.IntegerType.Create("ulong"), argumentTypes: new TypeCaster.CustomType[] { new TypeCaster.PointerType("void*") })},
        };
        if (!_withMultiThread)
        {
            // remove
            callReplacements["POOL_create"] = new CallReplacer.CallReplacementExpression("null", new TypeCaster.PointerType("void*"));
            callReplacements["POOL_add"] = new CallReplacer.CallReplacementRemove();
            callReplacements["POOL_free"] = new CallReplacer.CallReplacementRemove();
        }
        var traversalNames = new DirectoryInfo(_inputLocation)
            .GetFiles("*", SearchOption.AllDirectories)
            .Where(x => x.Name != "cpu.h")
            .Select(x => x.FullName)
            .ToHashSet();
        var inlineMethods = new HashSet<string>
        {
            "BIT_highbit32", "FSE_ctz", /*"FSE_minTableLog", */"HUF_fillDTableX2Level2", "HUF_DEltX1_set4", "MEM_read16",
            "MEM_read32", "MEM_read64", "MEM_readLE16", "MEM_readLE24", "MEM_readLE32", "MEM_readLE64",
            "MEM_readLEST", "MEM_readST", "MEM_write64", "MEM_writeLE16", "MEM_writeLE24", "MEM_writeLE32",
            "MEM_writeLE64", "MEM_writeLEST", "XXH_memcpy", "XXH_readLE32", "XXH_readLE64", "XXH64_mergeRound",
            "XXH64_round", /*"ZSTD_count", "ZSTD_decodeSequence",*/
            "ZSTD_hash3", "ZSTD_hash3Ptr", "ZSTD_hash4",
            "ZSTD_hash4Ptr", "ZSTD_hash5", "ZSTD_hash5Ptr", "ZSTD_hash6", "ZSTD_hash6Ptr", "ZSTD_hash7",
            "ZSTD_hash7Ptr", "ZSTD_hash8", "ZSTD_hash8Ptr", /*"ZSTD_hashPtr", */"ZSTD_highbit32",
            "ZSTD_NbCommonBytes", /*"ZSTD_storeSeq",*/
            // 1.5.0
            /*"ZSTD_updateDUBT", "ZSTD_insertDUBT1", "ZSTD_DUBT_findBestMatch", "ZSTD_BtFindBestMatch", "ZSTD_getLowestMatchIndex",*/
            "ZSTD_Vec128_read", "ZSTD_Vec128_set8", "ZSTD_Vec128_cmpMask8", "ZSTD_Vec256_read", "ZSTD_Vec256_set8", "ZSTD_Vec256_cmpMask8", "ZSTD_VecMask_next",
            "ZSTD_VecMask_rotateRight", /*"ZSTD_row_nextIndex", "ZSTD_row_prefetch", "ZSTD_row_nextCachedHash", "ZSTD_row_update_internal", "ZSTD_row_getMatchMask",*/
            "ZSTD_RowFindBestMatch_generic", "ZSTD_RowFindBestMatch_selectMLS",
            // 1.5.1
            "HUF_getNbBits", "HUF_getNbBitsFast", "HUF_getValue", "HUF_getValueFast", "HUF_getIndex", "HUF_addBits", "HUF_zeroIndex1", "HUF_mergeIndex1", "HUF_flushBits",
            "HUF_encodeSymbol", "HUF_buildDEltX2U32", "HUF_buildDEltX2U64",
            /*"ZSTD_copy16", */"HUF_buildDEltX2", "HUF_fillDTableX2ForWeight",
            "ZSTD_storeSeq", "ZSTD_compressBlock_fast_noDict_generic", "ZSTD_RowFindBestMatch", 
            // 1.5.2
            "BIT_lookBitsFast", "BIT_skipBits", "BIT_reloadDStreamFast", "HUF_decodeSymbolX2",
            "HUF_setNbBits", "HUF_setValue",
            // 1.5.3
            "ZSTD_copy16",
            // 1.5.4
            "FSE_decodeSymbol", "HUF_decodeSymbolX1", "ZSTD_decodeSequence",
            "XXH_readBE32", "XXH_readBE64",
            // 1.5.5
            "ZSTD_hash4PtrS", "ZSTD_hash5PtrS", "ZSTD_hash6PtrS", "ZSTD_hash7PtrS", "ZSTD_hash8PtrS",
        };

        foreach (var callsModifier in _callsModifiers)
        {
            foreach (var (name, callReplacement) in callsModifier.GetCallReplacements())
            {
                callReplacements[name] = callReplacement;
            }
        }

        IReadOnlySet<string>? sourceExcludeNames = null;
        if (!_withMultiThread)
        {
            sourceExcludeNames = new HashSet<string>(new[]
                {"JobThreadPool.cs", "SynchronizationWrapper.cs", "UnmanagedObject.cs", "Pool.cs"});
        }

        // inside of nested arrays, don't work in .NET Native
        var excludeFunctionPointers = new HashSet<string>
        {
            "ZSTD_getAllMatchesFn", "ZSTD_BlockCompressor_f",
        };

        var variableSizeTypes = new Dictionary<string, IVariableSizeType>
        {
            // sizeof(HUF_CTableHeader) == sizeof(nuint)
            {"HUF_CTableHeader", new OverrideSize(SyntaxFactory.IdentifierName("nuint"))},
        };
        var jitInlineMethods = new HashSet<string>
        {
            "ZSTD_match4Found_cmov", "ZSTD_match4Found_branch",
        };

        return new ProjectBuilderConfig(namespaceName, _outputLocation, _unsafeOutputLocation, _sourceLocation,
            remappedNames: remappedNames,
            excludedNames: unnecessarySymbols, traversalNames: traversalNames, inlineMethods: inlineMethods,
            callReplacements: callReplacements, structToClasses: structToClasses, sourceExcludeNames: sourceExcludeNames,
            excludeFunctionPointers: excludeFunctionPointers, variableSizeTypes: variableSizeTypes,
            jitInlineMethods: jitInlineMethods);
    }

    public async Task Generate()
    {
        var sw = Stopwatch.StartNew();

        var macroStorage = new MacroStorage();
        Console.WriteLine("CollectMacros");
        await CollectMacros(macroStorage);
        Console.WriteLine("Generate");
        await GenerateProject(macroStorage);
        Console.WriteLine("Modify");
        var projectModifier = new ProjectModifier(_projectBuilder, _projectBuilder.Reporter);
        projectModifier.ModifyProject();
        Console.WriteLine("Save");
        await _projectBuilder.Save();

        Console.WriteLine($"Finished in {sw.ElapsedMilliseconds} ms");
    }

    private async Task GenerateProject(MacroStorage macroStorage)
    {
        var translationFlags = CXTranslationUnit_Flags.CXTranslationUnit_IncludeAttributedTypes |
                               CXTranslationUnit_Flags.CXTranslationUnit_VisitImplicitAttributes;

        var overrideMacros = new Dictionary<string, string>
        {
            // remove internal helper
            { "_FORCE_HAS_FORMAT_STRING", "_FORCE_HAS_FORMAT_STRING(...) {}" },
            // branchless in .NET 8
            { "BOUNDED", "BOUNDED(min,val,max) ((val) <= (min) ? (min) : ((val) <= (max) ? (val) : (max)))"},
            // XXH_ASSUME - remove
            { "XXH_ASSUME", "XXH_ASSUME(c)"},
        };

        foreach (var callsModifier in _callsModifiers)
        {
            foreach (var (name, macro) in callsModifier.GetMacros())
            {
                overrideMacros[name] = macro;
            }
        }

        var macros = macroStorage.Macros;

        var originalFiles = new Dictionary<string, string>();
        var unsavedFiles = new Dictionary<string, string>();
        foreach (var kvp in overrideMacros)
        {
            if (macros.TryGetValue(kvp.Key, out var list))
            {
                foreach (var macroInfo in list)
                {
                    if (!originalFiles.TryGetValue(macroInfo.FileName, out var contents))
                    {
                        contents = await File.ReadAllTextAsync(macroInfo.FileName);
                        originalFiles[macroInfo.FileName] = contents;
                    }

                    var originalDefinition = contents.Substring((int) macroInfo.Offset,
                        (int) (macroInfo.OffsetEnd - macroInfo.Offset));

                    if (!unsavedFiles.TryGetValue(macroInfo.FileName, out var unsavedFileContents))
                    {
                        unsavedFileContents = contents;
                        unsavedFiles[macroInfo.FileName] = unsavedFileContents;
                    }

                    unsavedFiles[macroInfo.FileName] = unsavedFileContents.Replace(originalDefinition, kvp.Value);
                }
            }
        }

        var translationUnits = new Dictionary<string, TranslationUnit>();

        await Task.WhenAll(_fileNames.Select(async file =>
            await Task.Run(() =>
            {
                var filePath = Path.GetFullPath(Path.Combine(_inputLocation, file));

                var newUnsavedFiles = new Dictionary<string, string>(unsavedFiles);
                if (!newUnsavedFiles.TryGetValue(filePath, out var contents))
                {
                    contents = File.ReadAllText(filePath);
                }

                contents = string.Join("\r\n", _callsModifiers.Select(callsModifier => callsModifier.GetDefinitions())) +
                           "\r\n" + contents;

                newUnsavedFiles[filePath] = contents;

                var parser = new Parser(filePath, _clangCommandLineArgs.ToArray(), translationFlags,
                    newUnsavedFiles.Select(kvp => CXUnsavedFile.Create(kvp.Key, kvp.Value)).ToArray());

                var skipProcessing = false;

                if (!parser.Success)
                {
                    Console.WriteLine($"Error: Parsing failed for {filePath} due to {parser.ErrorCode}");
                    skipProcessing = true;
                }
                else if (parser.NumDiagnostics != 0)
                {
                    Console.WriteLine($"Diagnostics for {filePath}:");

                    for (uint i = 0; i < parser.NumDiagnostics; ++i)
                    {
                        using var diagnostic = parser.GetDiagnostic(i);

                        Console.Write("    ");
                        Console.WriteLine(diagnostic.Format(CXDiagnostic.DefaultDisplayOptions).ToString());

                        skipProcessing |= diagnostic.Severity == CXDiagnosticSeverity.CXDiagnostic_Error;
                        skipProcessing |= diagnostic.Severity == CXDiagnosticSeverity.CXDiagnostic_Fatal;
                    }
                }

                if (skipProcessing)
                {
                    Console.WriteLine($"Skipping {filePath} due to one or more errors listed above");
                    Console.WriteLine();
                    return;
                }

                var translationUnit = parser.GetTranslationUnit();
                lock (translationUnits)
                {
                    translationUnits.Add(file, translationUnit);
                }

            })));

        var codeGenerator = new CodeGenerator.CodeGenerator(_projectBuilder);
        foreach (var file in _fileNames)
        {
            var filePath = Path.GetFullPath(Path.Combine(_inputLocation, file));
            if (translationUnits.TryGetValue(file, out var translationUnit))
            {
                using (translationUnit)
                {
                    Console.WriteLine($"Processing {filePath}");
                    codeGenerator.Generate(translationUnit);
                }
            }
        }
    }

    private bool CollectMacrosForFile(MacroStorage macroStorage, string file)
    {
        var translationFlags = CXTranslationUnit_Flags.CXTranslationUnit_IncludeAttributedTypes |
                               CXTranslationUnit_Flags.CXTranslationUnit_VisitImplicitAttributes |
                               CXTranslationUnit_Flags.CXTranslationUnit_DetailedPreprocessingRecord;

        var macroCollector = new MacroCollector();

        var filePath = Path.Combine(_inputLocation, file);
        var parser = new Parser(filePath, _clangCommandLineArgs.ToArray(), translationFlags, Array.Empty<CXUnsavedFile>());

        var skipProcessing = false;

        if (!parser.Success)
        {
            Console.WriteLine($"Error: Parsing failed for {filePath} due to {parser.ErrorCode}");
            skipProcessing = true;
        }
        else if (parser.NumDiagnostics != 0)
        {
            Console.WriteLine($"Diagnostics for {filePath}:");

            for (uint i = 0; i < parser.NumDiagnostics; ++i)
            {
                using var diagnostic = parser.GetDiagnostic(i);

                Console.Write("    ");
                Console.WriteLine(diagnostic.Format(CXDiagnostic.DefaultDisplayOptions).ToString());

                skipProcessing |= diagnostic.Severity == CXDiagnosticSeverity.CXDiagnostic_Error;
                skipProcessing |= diagnostic.Severity == CXDiagnosticSeverity.CXDiagnostic_Fatal;
            }
        }

        if (skipProcessing)
        {
            Console.WriteLine($"Skipping {filePath} due to one or more errors listed above");
            Console.WriteLine();
            return false;
        }

        using var translationUnit = parser.GetTranslationUnit();
        Console.WriteLine($"Processing {filePath}");
        macroCollector.CollectMacros(translationUnit);
        lock (macroStorage)
        {
            macroStorage.Import(macroCollector.MacroStorage);
        }

        return true;
    }

    private async Task CollectMacros(MacroStorage macroStorage)
    {
        await Task.WhenAll(_fileNames.Select(async file =>
            await Task.Run(() => CollectMacrosForFile(macroStorage, file))));
    }
}

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ClangSharp;
using ClangSharp.Interop;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator;
using ZstdSharp.Generator.CodeGenerator.Macros;
using ZstdSharp.Generator.CodeGenerator.Reporter;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

// ReSharper disable StringLiteralTypo
// ReSharper disable CommentTypo

namespace ZstdSharp.Generator;

public class Generator
{
    private readonly string _inputLocation;
    private readonly string _outputLocation;
    private readonly string _unsafeOutputLocation;
    private readonly string _sourceLocation;
    private readonly List<string> _filenames;
    private readonly string[] _clangCommandLineArgs;
    private readonly ProjectBuilder _projectBuilder;

    public Generator(string inputLocation, string outputLocation)
    {
        _inputLocation = inputLocation;
        _outputLocation = outputLocation;
        _unsafeOutputLocation = Path.Combine(_outputLocation, "Unsafe");
        _sourceLocation = "Source";

        _filenames = new List<string>
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
            // decompress
            "decompress/huf_decompress.c",
            "decompress/zstd_ddict.c",
            "decompress/zstd_decompress.c",
            "decompress/zstd_decompress_block.c",
            // dictBuilder
            "dictBuilder/cover.c",
            "dictBuilder/fastcover.c",
            "dictBuilder/zdict.c"
        };
        _clangCommandLineArgs = new[]
        {
            "--language=c",
            "--std=c99",
            // turn off dynamic bmi2
            "-DDYNAMIC_BMI2=0",
            "-fparse-all-comments",
        };

        _projectBuilder = new ProjectBuilder(GetConfig(), new Reporter());

        CheckFiles();
    }

    private void CheckFiles()
    {
        foreach (var filename in _filenames)
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
            {"__m128i", "Vector128<sbyte>"}
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
            // ZDICT
            "g_selectivity_default", "ZDICT_clockSpan", "ZDICT_printHex", "ZDICT_getDictID", "ZDICT_getDictHeaderSize", "ZDICT_NbCommonBytes", "ZDICT_count", "ZDICT_initDictItem", "ZDICT_analyzePos", "isIncluded",
            "ZDICT_tryMerge", "ZDICT_removeDictItem", "ZDICT_insertDictItem", "ZDICT_dictSize", "ZDICT_trainBuffer_legacy", "ZDICT_fillNoise", "ZDICT_trainFromBuffer_unsafe_legacy", "ZDICT_trainFromBuffer_legacy",
            // ZstdLazy
            "ZSTD_rotateRight_U64", "ZSTD_rotateRight_U32", "ZSTD_rotateRight_U16",
            // ErrorPrivate
            "_force_has_format_string",
            // 1.5.3
            "ZSTD_cpuSupportsBmi2",
        };
        var callReplacements = new Dictionary<string, CallReplacer.CallReplacement>
        {
            {"MEM_64bits", new CallReplacer.CallReplacementExpression("MEM_64bits", new TypeCaster.BoolType())},
            {"MEM_32bits", new CallReplacer.CallReplacementExpression("MEM_32bits", new TypeCaster.BoolType())},
            {"MEM_isLittleEndian", new CallReplacer.CallReplacementExpression("BitConverter.IsLittleEndian", new TypeCaster.BoolType(), "System")},
            {"ZSTD_cpuid_bmi2", new CallReplacer.CallReplacementExpression("0", TypeCaster.IntegerType.Create("int"))},
            {"IsLittleEndian", new CallReplacer.CallReplacementExpression("BitConverter.IsLittleEndian", new TypeCaster.BoolType(), "System")},
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
            {"memset", new CallReplacer.CallReplacementInvocation("memset",new TypeCaster.VoidType(),
                "static ZstdSharp.UnsafeHelper", new TypeCaster.CustomType[] { new TypeCaster.PointerType("void*"), TypeCaster.IntegerType.Create("byte"), TypeCaster.IntegerType.Create("uint") })},
            {"memcpy", new CallReplacer.CallReplacementInvocation("memcpy",new TypeCaster.VoidType(),
                "static ZstdSharp.UnsafeHelper", new TypeCaster.CustomType[] { new TypeCaster.PointerType("void*"), new TypeCaster.PointerType("void*"), TypeCaster.IntegerType.Create("uint") })},
            {"ZSTD_cpuSupportsBmi2", new CallReplacer.CallReplacementExpression("0", TypeCaster.IntegerType.Create("int"))},
            // bool
            {"ERR_isError", new CallReplacer.CallReplacementIdentity(new TypeCaster.BoolType())},
            {"HUF_isError", new CallReplacer.CallReplacementIdentity(new TypeCaster.BoolType())},
            {"FSE_isError", new CallReplacer.CallReplacementIdentity(new TypeCaster.BoolType())},
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
            // remove
            {"POOL_create", new CallReplacer.CallReplacementExpression("null", new TypeCaster.PointerType("void*"))},
            {"POOL_add", new CallReplacer.CallReplacementRemove()},
            {"POOL_free", new CallReplacer.CallReplacementRemove()},
            // vector
            {"_mm_set1_epi8", new CallReplacer.CallReplacementInvocation("Vector128.Create", new TypeCaster.Vector128Type("Vector128<sbyte>"), "System.Runtime.Intrinsics")},
            {"_mm_loadu_si128", new CallReplacer.CallReplacementInvocation("Sse2.LoadVector128", new TypeCaster.Vector128Type("Vector128<sbyte>"), "System.Runtime.Intrinsics.X86",
                new TypeCaster.CustomType[] { new TypeCaster.PointerType("sbyte*") })},
            {"_mm_cmpeq_epi8", new CallReplacer.CallReplacementInvocation("Sse2.CompareEqual", new TypeCaster.Vector128Type("Vector128<sbyte>"), "System.Runtime.Intrinsics.X86")},
            {"_mm_movemask_epi8", new CallReplacer.CallReplacementInvocation("Sse2.MoveMask", TypeCaster.IntegerType.Create("int"), "System.Runtime.Intrinsics.X86")},
            // fastcover
            {"clock", new CallReplacer.CallReplacementRemove()},
        };
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
            // 
            //"BIT_reloadDStream", 
            //"ERR_isError", 
            //"ZSTD_decodeSequence",
        };

        return new ProjectBuilderConfig(namespaceName, _outputLocation, _unsafeOutputLocation, _sourceLocation, remappedNames: remappedNames,
            excludedNames: unnecessarySymbols, traversalNames: traversalNames, inlineMethods: inlineMethods,
            callReplacements: callReplacements, structToClasses: structToClasses);
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
        ModifyProject();
        Console.WriteLine("Save");
        await _projectBuilder.Save();

        Console.WriteLine($"Finished in {sw.ElapsedMilliseconds} ms");
    }

    private void ModifyMethod(string name, Func<FileBuilder, MethodDeclarationSyntax, MethodDeclarationSyntax?> modifier)
    {
        if (_projectBuilder.TryGetMethod(name, out var builder, out var methodDeclarationSyntax))
        {
            builder.ReplaceMethod(name, modifier(builder, methodDeclarationSyntax));
        }
        else
        {
            Console.WriteLine($"Error: No method {name}");
        }
    }

    private static BlockSyntax ParseBody(string body) => (BlockSyntax)SyntaxFactory.ParseStatement($"{{{body}}}");

    private static SyntaxList<TSyntax> WrapWithIfDefined<TSyntax>(SyntaxList<TSyntax> statements, string name) where TSyntax : SyntaxNode
    {
        var firstStatement = statements.First();
        statements = statements.Replace(firstStatement, firstStatement
            .WithLeadingTrivia(SyntaxFactory.Trivia(
                SyntaxFactory.IfDirectiveTrivia(
                    SyntaxFactory.IdentifierName(name),
                    true,
                    false,
                    false))));

        var lastStatement = statements.Last();
        return statements.Replace(lastStatement, lastStatement
            .WithTrailingTrivia(SyntaxFactory.Trivia(
                SyntaxFactory.EndIfDirectiveTrivia(
                    true))));
    }

    private static TSyntax WrapWithIfDefined<TSyntax>(TSyntax statement, string name) where TSyntax : SyntaxNode
    {
        return statement
            .WithLeadingTrivia(SyntaxFactory.Trivia(
                SyntaxFactory.IfDirectiveTrivia(
                    SyntaxFactory.IdentifierName(name),
                    true,
                    false,
                    false)))
            .WithTrailingTrivia(SyntaxFactory.Trivia(
                SyntaxFactory.EndIfDirectiveTrivia(
                    true)));
    }

    // todo made for zstd 1.5.2
    private void ModifyProject()
    {
        ModifyMethod("ZSTD_highbit32", (builder, method) =>
        {
            builder.AddUsingDirective("System.Numerics");
            return method.WithBody(ParseBody("assert(val != 0);return (uint) BitOperations.Log2(val);"));
        });

        ModifyMethod("ZSTD_countTrailingZeros", (builder, method) =>
        {
            builder.AddUsingDirective("System.Numerics");
            return method.WithBody(ParseBody("assert(val != 0);return (uint) BitOperations.TrailingZeroCount(val);"));
        });

        ModifyMethod("FSE_ctz", (builder, method) =>
        {
            builder.AddUsingDirective("System.Numerics");
            return method.WithBody(ParseBody("assert(val != 0);return (uint) BitOperations.TrailingZeroCount(val);"));
        });

        ModifyMethod("ZSTD_NbCommonBytes", (builder, method) =>
        {
            builder.AddUsingDirective("System.Numerics");
            return method.WithBody(ParseBody(
                "assert(val != 0);if (BitConverter.IsLittleEndian){return (uint)(BitOperations.TrailingZeroCount(val) >> 3);} return (uint)(BitOperations.Log2(val) >> 3);"));
        });

        ModifyMethod("ZSTD_VecMask_next", (builder, method) =>
        {
            builder.AddUsingDirective("System.Numerics");
            return method.WithBody(ParseBody("assert(val != 0);return (uint) BitOperations.TrailingZeroCount(val);"));
        });

        ModifyMethod("BIT_highbit32", (builder, method) =>
        {
            builder.AddUsingDirective("System.Numerics");
            return method.WithBody(ParseBody("assert(val != 0);return (uint) BitOperations.Log2(val);"));
        });

        ModifyMethod("XXH_memcpy", (_, method) => method.WithBody(ParseBody("memcpy(dest, src, (uint)size);")));

        ModifyMethod("ZSTD_copy16", (_, method) => method.WithBody(ParseBody("memcpy(dst, src, 16);")));

        ModifyMethod("ZSTD_row_getSSEMask", (_, method) => WrapWithIfDefined(method, "NETCOREAPP3_0_OR_GREATER"));

        ModifyMethod("BIT_getMiddleBits", (builder, method) =>
        {
            var body = method.Body!;
            builder.AddUsingDirective("System.Runtime.Intrinsics.X86");
            var returnStatement = body.Statements.First(s => s is ReturnStatementSyntax);

            var block = ParseBody(
                "if (Bmi2.X64.IsSupported) {return (nuint) Bmi2.X64.ZeroHighBits(bitContainer >> (int) (start & regMask), nbBits);}if (Bmi2.IsSupported) {return Bmi2.ZeroHighBits((uint) (bitContainer >> (int) (start & regMask)), nbBits);}");

            var statements = WrapWithIfDefined(block.Statements, "NETCOREAPP3_1_OR_GREATER");
            return method.WithBody(body.InsertNodesBefore(returnStatement, statements));
        });

        ModifyMethod("BIT_getLowerBits", (builder, method) =>
        {
            var body = method.Body!;
            builder.AddUsingDirective("System.Runtime.Intrinsics.X86");
            var returnStatement = body.Statements.First(s => s is ReturnStatementSyntax);

            var block = ParseBody(
                "if (Bmi2.X64.IsSupported) {return (nuint) Bmi2.X64.ZeroHighBits(bitContainer, nbBits);}if (Bmi2.IsSupported) {return Bmi2.ZeroHighBits((uint)bitContainer, nbBits);}");

            var statements = WrapWithIfDefined(block.Statements, "NETCOREAPP3_1_OR_GREATER");
            return method.WithBody(body.InsertNodesBefore(returnStatement, statements));
        });

        ModifyMethod("ZSTD_buildSequencesStatistics", (_, method) => AddSkipInit(method, "stats"));

        ModifyMethod("ZSTD_compressBlock_opt_generic", (_, method) => AddSkipInit(method, "lastSequence"));

        ModifyMethod("ZSTD_decompressSequences_bodySplitLitBuffer", (_, method) => AddSkipInit(method, "seqState"));

        ModifyMethod("ZSTD_decompressSequences_body", (_, method) => AddSkipInit(method, "seqState"));

        ModifyMethod("ZSTD_decompressSequencesLong_body", (_, method) => AddSkipInit(method, "seqState"));

        ModifyMethod("ZSTD_encodeSequences_body",
            (_, method) => method
                .WithAttributeLists(SyntaxFactory.List(method.AttributeLists.Where(attributeList =>
                    !attributeList.Attributes.Any(attribute =>
                        attribute.ToString().Contains("MethodImplOptions.AggressiveInlining"))))));

        ModifyMethod("ZSTD_hashPtr", (_, method) => ConvertMethodSwitchToIfs(method));

        ModifyMethod("ZSTD_row_getMatchMask", (builder, method) =>
        {
            var body = method.Body!;
            var returnStatement = body.Statements.First(s => s is ReturnStatementSyntax);

            // conditional sse implementation
            var statements = new List<SyntaxNode>
            {
                WrapWithIfDefined(SyntaxFactory.IfStatement(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.IdentifierName("Sse2"),
                            SyntaxFactory.IdentifierName("IsSupported")),
                        SyntaxFactory.Block(
                            SyntaxFactory.SingletonList(
                                returnStatement))), "NETCOREAPP3_0_OR_GREATER")
            };

            // arm implementation
            var armImplementation = ParseBody(
                "/* This NEON path only works for little endian - otherwise use SWAR below */\r\n            if (AdvSimd.IsSupported && BitConverter.IsLittleEndian)\r\n            {\r\n                if (rowEntries == 16)\r\n                {\r\n                    Vector128<byte> chunk = AdvSimd.LoadVector128(src);\r\n                    Vector128<UInt16> equalMask = AdvSimd.CompareEqual(chunk, AdvSimd.DuplicateToVector128(tag)).As<byte, UInt16>();\r\n                    Vector128<UInt16> t0 = AdvSimd.ShiftLeftLogical(equalMask, 7);\r\n                    Vector128<UInt32> t1 = AdvSimd.ShiftRightAndInsert(t0, t0, 14).As<UInt16, UInt32>();\r\n                    Vector128<UInt64> t2 = AdvSimd.ShiftRightLogical(t1, 14).As<UInt32, UInt64>();\r\n                    Vector128<byte> t3 = AdvSimd.ShiftRightLogicalAdd(t2, t2, 28).As<UInt64, byte>();\r\n                    ushort hi = AdvSimd.Extract(t3, 8);\r\n                    ushort lo = AdvSimd.Extract(t3, 0);\r\n                    return BitOperations.RotateRight((ushort)((hi << 8) | lo), (int)head);\r\n                }\r\n                else if (rowEntries == 32)\r\n                {\r\n                    // todo, there is no vld2q_u16 in c#\r\n                }\r\n                else\r\n                { /* rowEntries == 64 */\r\n                    // todo, there is no vld4q_u8 in c#\r\n                }\r\n            }\r\n");
            builder.AddUsingDirective("System.Runtime.Intrinsics.Arm");
            var armImplementationStatements = WrapWithIfDefined(armImplementation.Statements, "NET5_0_OR_GREATER");
            statements.AddRange(armImplementationStatements);

            // soft implementation
            var softImplementation = ParseBody(
                "nuint chunkSize = (nuint)sizeof(nuint);\r\n                nuint shiftAmount = chunkSize * 8 - chunkSize;\r\n                nuint xFF = ~(nuint)0;\r\n                nuint x01 = xFF / 0xFF;\r\n                nuint x80 = x01 << 7;\r\n                nuint splatChar = tag * x01;\r\n                ulong matches = 0;\r\n                int i = (int)(rowEntries - chunkSize);\r\n                assert(sizeof(nuint) == 4 || sizeof(nuint) == 8);\r\n                if (BitConverter.IsLittleEndian)\r\n                {\r\n                    nuint extractMagic = xFF / 0x7F >> (int)chunkSize;\r\n                    do\r\n                    {\r\n                        nuint chunk = MEM_readST(&src[i]);\r\n                        chunk ^= splatChar;\r\n                        chunk = ((chunk | x80) - x01 | chunk) & x80;\r\n                        matches <<= (int)chunkSize;\r\n                        matches |= (ulong)(chunk * extractMagic >> (int)shiftAmount);\r\n                        i -= (int)chunkSize;\r\n                    }\r\n                    while (i >= 0);\r\n                }\r\n                else\r\n                {\r\n                    nuint msb = xFF ^ xFF >> 1;\r\n                    nuint extractMagic = msb / 0x1FF | msb;\r\n                    do\r\n                    {\r\n                        nuint chunk = MEM_readST(&src[i]);\r\n                        chunk ^= splatChar;\r\n                        chunk = ((chunk | x80) - x01 | chunk) & x80;\r\n                        matches <<= (int)chunkSize;\r\n                        matches |= (ulong)((chunk >> 7) * extractMagic >> (int)shiftAmount);\r\n                        i -= (int)chunkSize;\r\n                    }\r\n                    while (i >= 0);\r\n                }\r\n\r\n                matches = ~matches;\r\n                if (rowEntries == 16)\r\n                {\r\n                    return BitOperations.RotateRight((ushort)matches, (int)head);\r\n                }\r\n                else if (rowEntries == 32)\r\n                {\r\n                    return BitOperations.RotateRight((uint)matches, (int)head);\r\n                }\r\n                else\r\n                {\r\n                    return BitOperations.RotateRight((ulong)matches, (int)head);\r\n                }");
            builder.AddUsingDirective("System", "System.Numerics");
            statements.Add(softImplementation);

            return method.WithBody(body
                .ReplaceNode(returnStatement, statements));
        });
    }

    private static List<StatementSyntax>? ConvertSwitchToIfs(SwitchStatementSyntax switchStatement)
    {
        var statements = new List<StatementSyntax>();
        var hasBreak = false;
        StatementSyntax? defaultStatement = null;

        foreach (var section in switchStatement.Sections)
        {
            // goto is not supported
            if (section.Statements.Any(statement => statement is GotoStatementSyntax))
            {
                return null;
            }

            ExpressionSyntax? condition = null;

            foreach (var label in section.Labels)
            {
                if (label is DefaultSwitchLabelSyntax)
                {
                    // default label
                    condition = null;
                    break;
                }

                if (label is CaseSwitchLabelSyntax caseSwitchLabelSyntax)
                {
                    var labelExpression = SyntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression, switchStatement.Expression,
                        caseSwitchLabelSyntax.Value);

                    if (condition == null)
                    {
                        condition = labelExpression;
                    }
                    else
                    {
                        condition = SyntaxFactory.BinaryExpression(SyntaxKind.LogicalOrExpression,
                            condition, labelExpression);
                    }
                }
            }

            if (condition != null && section.Statements.Any(statement => statement is BreakStatementSyntax))
            {
                hasBreak = true;
            }

            var innerStatements = section.Statements.Where(statement => statement is not BreakStatementSyntax).ToList();
            var innerStatement = innerStatements.Count switch
            {
                0 => null,
                1 => innerStatements.First(),
                _ => SyntaxFactory.Block().WithStatements(SyntaxFactory.List(innerStatements))
            };

            if (condition != null)
            {
                statements.Add(SyntaxFactory.IfStatement(condition, innerStatement ?? SyntaxFactory.EmptyStatement()));
            }
            else
            {
                defaultStatement = innerStatement;
            }
        }

        // default to end
        if (defaultStatement != null)
        {
            statements.Add(defaultStatement);
        }

        // no breaks -> if () {}, ...
        if (!hasBreak)
        {
            return statements;
        }

        // has break -> if () {} else if (...) else if () ... else ..
        StatementSyntax? currentStatement = null;
        foreach (var statement in statements.AsEnumerable().Reverse())
        {
            if (currentStatement != null && statement is IfStatementSyntax ifStatementSyntax)
            {
                currentStatement = ifStatementSyntax.WithElse(SyntaxFactory.ElseClause(currentStatement));
            }
            else
            {
                currentStatement = statement;
            }
        }

        statements.Clear();
        if (currentStatement != null)
        {
            statements.Add(currentStatement);
        }

        return statements;

    }

    private static MethodDeclarationSyntax ConvertMethodSwitchToIfs(MethodDeclarationSyntax method)
    {
        foreach (var node in method.DescendantNodes())
        {
            if (node is SwitchStatementSyntax switchStatement)
            {
                var statements = ConvertSwitchToIfs(switchStatement);
                if (statements == null)
                {
                    Console.WriteLine("Failed to convert switch to ifs");
                    return method;
                }

                if (switchStatement.Parent is not BlockSyntax &&
                    !(statements.Count == 1 && statements.FirstOrDefault() is BlockSyntax))
                {
                    statements = new List<StatementSyntax>
                        {SyntaxFactory.Block().WithStatements(SyntaxFactory.List(statements))};
                }

                return method.ReplaceNode(switchStatement, statements);
            }
        }

        return method;
    }

    private static MethodDeclarationSyntax AddSkipInit(MethodDeclarationSyntax method, string varName)
    {
        foreach (var node in method.DescendantNodes())
        {
            if (node is LocalDeclarationStatementSyntax localDeclarationStatement)
            {
                foreach (var v in localDeclarationStatement.Declaration.Variables)
                {
                    if (v.Identifier.ToString() == varName && v.Initializer == null)
                    {
                        var callSkipInit = SyntaxFactory.ExpressionStatement(
                            SyntaxFactory.InvocationExpression(
                                    SyntaxFactory.IdentifierName("SkipInit"))
                                .WithArgumentList(
                                    SyntaxFactory.ArgumentList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.Argument(
                                                    SyntaxFactory.IdentifierName(v.Identifier))
                                                .WithRefOrOutKeyword(
                                                    SyntaxFactory.Token(SyntaxKind.OutKeyword))))));

                        method = method.InsertNodesAfter(node, new[] { callSkipInit });
                        break;
                    }
                }
            }
        }

        return method;
    }

    private async Task GenerateProject(MacroStorage macroStorage)
    {
        var translationFlags = CXTranslationUnit_Flags.CXTranslationUnit_IncludeAttributedTypes |
                               CXTranslationUnit_Flags.CXTranslationUnit_VisitImplicitAttributes;

        var overrideMacros = new Dictionary<string, string>
        {
            { "assert", "assert(c) assert(c)" },
            { "XXH_STATIC_ASSERT", "XXH_STATIC_ASSERT(c) assert(c)" },
            { "XXH_CPU_LITTLE_ENDIAN", "XXH_CPU_LITTLE_ENDIAN IsLittleEndian()" },
            { "_FORCE_HAS_FORMAT_STRING", "_FORCE_HAS_FORMAT_STRING(...) {}" },
            { "PREFETCH_L1", "PREFETCH_L1(ptr) Prefetch0(ptr)"},
            { "PREFETCH_L2", "PREFETCH_L2(ptr) Prefetch1(ptr)"},
            // fastcover
            { "DISPLAY", "DISPLAY(...) {}" },
            { "DISPLAYLEVEL", "DISPLAYLEVEL(...) {}" },
            { "DISPLAYUPDATE", "DISPLAYUPDATE(...) {}" },
        };

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

        await Task.WhenAll(_filenames.Select(async file =>
            await Task.Run(() =>
            {
                var filePath = Path.GetFullPath(Path.Combine(_inputLocation, file));

                var newUnsavedFiles = new Dictionary<string, string>(unsavedFiles);
                if (!newUnsavedFiles.TryGetValue(filePath, out var contents))
                {
                    contents = File.ReadAllText(filePath);
                }

                contents = "int IsLittleEndian();\r\nvoid assert(int c);\r\nvoid Prefetch0(const void* ptr);\r\nvoid Prefetch1(const void* ptr);\r\n\r\n" + contents;

                newUnsavedFiles[filePath] = contents;

                var parser = new Parser(filePath, _clangCommandLineArgs, translationFlags,
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
        foreach (var file in _filenames)
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
        var parser = new Parser(filePath, _clangCommandLineArgs, translationFlags, Array.Empty<CXUnsavedFile>());

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
        await Task.WhenAll(_filenames.Select(async file =>
            await Task.Run(() => CollectMacrosForFile(macroStorage, file))));
    }
}

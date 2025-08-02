using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using ZstdSharp.Generator.CodeGenerator;
using ZstdSharp.Generator.CodeGenerator.Reporter;

// ReSharper disable StringLiteralTypo
// ReSharper disable CommentTypo

namespace ZstdSharp.Generator.Modify;

internal class ProjectModifier(ProjectBuilder projectBuilder, IReporter reporter)
{
    private void ModifyMethod(string name,
        Func<FileBuilder, MethodDeclarationSyntax, MethodDeclarationSyntax?> modifier)
    {
        if (!projectBuilder.ModifyMethod(name, modifier))
        {
            reporter.Report(DiagnosticLevel.Error, $"No method {name}");
        }
    }

    private static BlockSyntax ParseBody(string body) => (BlockSyntax)SyntaxFactory.ParseStatement($"{{{body}}}");

    internal static SyntaxList<TSyntax> WrapWithIfDefined<TSyntax>(SyntaxList<TSyntax> statements, string name)
        where TSyntax : SyntaxNode
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

    internal static TSyntax WrapWithIfDefined<TSyntax>(TSyntax statement, string name) where TSyntax : SyntaxNode
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
                    var labelExpression = SyntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression,
                        switchStatement.Expression,
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

    private MethodDeclarationSyntax ConvertMethodSwitchToIfs(MethodDeclarationSyntax method)
    {
        foreach (var node in method.DescendantNodes())
        {
            if (node is SwitchStatementSyntax switchStatement)
            {
                var statements = ConvertSwitchToIfs(switchStatement);
                if (statements == null)
                {
                    reporter.Report(DiagnosticLevel.Error, "Failed to convert switch to ifs");
                    return method;
                }

                if (switchStatement.Parent is not BlockSyntax &&
                    !(statements.Count == 1 && statements.FirstOrDefault() is BlockSyntax))
                {
                    statements = [SyntaxFactory.Block().WithStatements(SyntaxFactory.List(statements))];
                }

                return method.ReplaceNode(switchStatement, statements);
            }
        }

        return method;
    }

    private class AddSkipInitRewriter : CSharpSyntaxRewriter
    {
        private HashSet<string>? _varNames;

        public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            List<StatementSyntax>? additionalStatements = null;

            foreach (var v in node.Declaration.Variables)
            {
                if (_varNames!.Remove(v.Identifier.ToString()) && v.Initializer == null)
                {
                    var callSkipInit = SyntaxFactory.ExpressionStatement(
                        SyntaxFactory.InvocationExpression(
                                SyntaxFactory.IdentifierName("System.Runtime.CompilerServices.Unsafe.SkipInit"))
                            .WithArgumentList(
                                SyntaxFactory.ArgumentList(
                                    SyntaxFactory.SingletonSeparatedList(
                                        SyntaxFactory.Argument(
                                                SyntaxFactory.IdentifierName(v.Identifier))
                                            .WithRefOrOutKeyword(
                                                SyntaxFactory.Token(SyntaxKind.OutKeyword))))));

                    additionalStatements ??= new List<StatementSyntax>();
                    additionalStatements.Add(callSkipInit);
                }
            }

            if (additionalStatements != null)
            {
                return FoldBlockHelper.CombineStatements([node, ..additionalStatements]);
            }

            return base.VisitLocalDeclarationStatement(node);
        }

        public MethodDeclarationSyntax Run(MethodDeclarationSyntax method, IEnumerable<string> varNames)
        {
            _varNames = [..varNames];
            return (FoldBlockHelper.FoldBlocks(Visit(method)) as MethodDeclarationSyntax)!;
        }
    }

    internal static MethodDeclarationSyntax AddSkipInit(MethodDeclarationSyntax method, IEnumerable<string> varNames) 
        => new AddSkipInitRewriter().Run(method, varNames);

    internal static MethodDeclarationSyntax AddSkipInit(MethodDeclarationSyntax method, string varName)
        => AddSkipInit(method, [varName]);

    private void GuardAsserts()
    {
        foreach (var methodName in projectBuilder.GetMethods())
        {
            ModifyMethod(methodName, (builder, method) => new AssertsRewriter(builder).RewriteMethod(method));
        }
    }

    private void ModifyVectorCode()
    {
        /* SSE2 is available for .NET Core 3+, ARM for .NET 5/9+, software implementation otherwise */
        ModifyMethod("ZSTD_row_getMatchMask", (builder, method) =>
        {
            var body = method.Body!;
            var returnStatement = body.Statements.First(s => s is ReturnStatementSyntax);

            // conditional sse implementation
            var statements = new List<SyntaxNode>
            {
                WrapWithIfDefined(SyntaxFactory.IfStatement(
                    SyntaxFactory.IdentifierName("Sse2.IsSupported"),
                    SyntaxFactory.Block(
                        SyntaxFactory.SingletonList(
                            returnStatement))), "NETCOREAPP3_0_OR_GREATER")
            };

            // arm implementation
            var armImplementation = ParseBody(@"/* This NEON path only works for little endian - otherwise use SWAR below */
            if (AdvSimd.IsSupported && BitConverter.IsLittleEndian)
            {
                if (rowEntries == 16)
                {
                    /* vshrn_n_u16 shifts by 4 every u16 and narrows to 8 lower bits.
                     * After that groups of 4 bits represent the equalMask. We lower
                     * all bits except the highest in these groups by doing AND with
                     * 0x88 = 0b10001000.
                     */
                    Vector128<byte> chunk = AdvSimd.LoadVector128(src);
                    Vector128<ushort> equalMask = AdvSimd.CompareEqual(chunk, AdvSimd.DuplicateToVector128(tag)).As<byte, ushort>();
                    Vector64<byte> res = AdvSimd.ShiftRightLogicalNarrowingLower(equalMask, 4);
                    ulong matches = res.As<byte, ulong>().GetElement(0);
                    return BitOperations.RotateRight(matches, (int)headGrouped) & 0x8888888888888888;
                }
                else if (rowEntries == 32)
                {
#if NET9_0_OR_GREATER
                    if (AdvSimd.Arm64.IsSupported)
                    {
                        /* Same idea as with rowEntries == 16 but doing AND with
                         * 0x55 = 0b01010101.
                         */
                        (Vector128<ushort> chunk0, Vector128<ushort> chunk1) = AdvSimd.Arm64.Load2xVector128AndUnzip((ushort*)src);
                        Vector128<byte> dup = AdvSimd.DuplicateToVector128(tag);
                        Vector64<byte> t0 = AdvSimd.ShiftRightLogicalNarrowingLower(AdvSimd.CompareEqual(chunk0.As<ushort, byte>(), dup).As<byte, ushort>(), 6);
                        Vector64<byte> t1 = AdvSimd.ShiftRightLogicalNarrowingLower(AdvSimd.CompareEqual(chunk1.As<ushort, byte>(), dup).As<byte, ushort>(), 6);
                        Vector64<byte> res = AdvSimd.ShiftLeftAndInsert(t0, t1, 4);
                        ulong matches = res.As<byte, ulong>().GetElement(0);
                        return BitOperations.RotateRight(matches, (int)headGrouped) & 0x5555555555555555;
                    }
#endif
                }
                else
                { /* rowEntries == 64 */
#if NET9_0_OR_GREATER
                    if (AdvSimd.Arm64.IsSupported)
                    {
                        (Vector128<byte> chunk0, Vector128<byte> chunk1, Vector128<byte> chunk2, Vector128<byte> chunk3) = AdvSimd.Arm64.Load4xVector128AndUnzip(src);
                        Vector128<byte> dup = AdvSimd.DuplicateToVector128(tag);
                        Vector128<byte> cmp0 = AdvSimd.CompareEqual(chunk0, dup);
                        Vector128<byte> cmp1 = AdvSimd.CompareEqual(chunk1, dup);
                        Vector128<byte> cmp2 = AdvSimd.CompareEqual(chunk2, dup);
                        Vector128<byte> cmp3 = AdvSimd.CompareEqual(chunk3, dup);

                        Vector128<byte> t0 = AdvSimd.ShiftRightAndInsert(cmp1, cmp0, 1);
                        Vector128<byte> t1 = AdvSimd.ShiftRightAndInsert(cmp3, cmp2, 1);
                        Vector128<byte> t2 = AdvSimd.ShiftRightAndInsert(t1, t0, 2);
                        Vector128<byte> t3 = AdvSimd.ShiftRightAndInsert(t2, t2, 4);
                        Vector64<byte> t4 = AdvSimd.ShiftRightLogicalNarrowingLower(t3.As<byte, ushort>(), 4);
                        ulong matches = t4.As<byte, ulong>().GetElement(0);
                        return BitOperations.RotateRight(matches, (int) headGrouped);
                    }
#endif
                }
            }");
            builder.AddUsingDirective("System.Runtime.Intrinsics.Arm");
            var armImplementationStatements = WrapWithIfDefined(armImplementation.Statements, "NET5_0_OR_GREATER");
            statements.AddRange(armImplementationStatements);

            // soft implementation
            var softImplementation = ParseBody(@"
                nuint chunkSize = (nuint)sizeof(nuint);
                nuint shiftAmount = chunkSize * 8 - chunkSize;
                nuint xFF = ~(nuint)0;
                nuint x01 = xFF / 0xFF;
                nuint x80 = x01 << 7;
                nuint splatChar = tag * x01;
                ulong matches = 0;
                int i = (int)(rowEntries - chunkSize);
                assert(sizeof(nuint) == 4 || sizeof(nuint) == 8);
                if (BitConverter.IsLittleEndian)
                {
                    nuint extractMagic = xFF / 0x7F >> (int)chunkSize;
                    do
                    {
                        nuint chunk = MEM_readST(&src[i]);
                        chunk ^= splatChar;
                        chunk = ((chunk | x80) - x01 | chunk) & x80;
                        matches <<= (int)chunkSize;
                        matches |= chunk * extractMagic >> (int)shiftAmount;
                        i -= (int)chunkSize;
                    }
                    while (i >= 0);
                }
                else
                {
                    nuint msb = xFF ^ xFF >> 1;
                    nuint extractMagic = msb / 0x1FF | msb;
                    do
                    {
                        nuint chunk = MEM_readST(&src[i]);
                        chunk ^= splatChar;
                        chunk = ((chunk | x80) - x01 | chunk) & x80;
                        matches <<= (int)chunkSize;
                        matches |= (chunk >> 7) * extractMagic >> (int)shiftAmount;
                        i -= (int)chunkSize;
                    }
                    while (i >= 0);
                }

                matches = ~matches;
                if (rowEntries == 16)
                {
                    return BitOperations.RotateRight((ushort)matches, (int)headGrouped);
                }
                else if (rowEntries == 32)
                {
                    return BitOperations.RotateRight((uint)matches, (int)headGrouped);
                }
                else
                {
                    return BitOperations.RotateRight(matches, (int)headGrouped);
                }");
            builder.AddUsingDirective("System", "System.Numerics");
            statements.Add(softImplementation);

            return method.WithBody(body
                .ReplaceNode(returnStatement, statements));
        });

        // ARM version
        ModifyMethod("ZSTD_row_matchMaskGroupWidth", (_, method) =>
            method.WithBody(ParseBody(@"
            assert(rowEntries == 16 || rowEntries == 32 || rowEntries == 64);
            assert(rowEntries <= 64);

#if NET5_0_OR_GREATER
            if (AdvSimd.IsSupported && BitConverter.IsLittleEndian)
            {
                if (rowEntries == 16)
                    return 4;
#if NET9_0_OR_GREATER
                if (AdvSimd.Arm64.IsSupported)
                {
                    if (rowEntries == 32)
                        return 2;
                    if (rowEntries == 64)
                        return 1;
                }
#endif
            }
#endif
            return 1;"))
        );

        // unrolled SSE2 version
        ModifyMethod("ZSTD_row_getSSEMask", (_, method) =>
            WrapWithIfDefined(method.WithBody(ParseBody(@"
            Vector128<byte> comparisonMask = Vector128.Create(tag);
            assert(nbChunks is 1 or 2 or 4);
            if (nbChunks == 1)
            {
                Vector128<byte> chunk0 = Sse2.LoadVector128(src);
                Vector128<byte> equalMask0 = Sse2.CompareEqual(chunk0, comparisonMask);
                int matches0 = Sse2.MoveMask(equalMask0);
                return BitOperations.RotateRight((ushort)matches0, (int)head);
            }

            if (nbChunks == 2)
            {
                Vector128<byte> chunk0 = Sse2.LoadVector128(src);
                Vector128<byte> equalMask0 = Sse2.CompareEqual(chunk0, comparisonMask);
                int matches0 = Sse2.MoveMask(equalMask0);
                Vector128<byte> chunk1 = Sse2.LoadVector128(src + 16);
                Vector128<byte> equalMask1 = Sse2.CompareEqual(chunk1, comparisonMask);
                int matches1 = Sse2.MoveMask(equalMask1);
                return BitOperations.RotateRight((uint)matches1 << 16 | (uint)matches0, (int)head);
            }

            {
                Vector128<byte> chunk0 = Sse2.LoadVector128(src);
                Vector128<byte> equalMask0 = Sse2.CompareEqual(chunk0, comparisonMask);
                int matches0 = Sse2.MoveMask(equalMask0);
                Vector128<byte> chunk1 = Sse2.LoadVector128(src + 16 * 1);
                Vector128<byte> equalMask1 = Sse2.CompareEqual(chunk1, comparisonMask);
                int matches1 = Sse2.MoveMask(equalMask1);
                Vector128<byte> chunk2 = Sse2.LoadVector128(src + 16 * 2);
                Vector128<byte> equalMask2 = Sse2.CompareEqual(chunk2, comparisonMask);
                int matches2 = Sse2.MoveMask(equalMask2);
                Vector128<byte> chunk3 = Sse2.LoadVector128(src + 16 * 3);
                Vector128<byte> equalMask3 = Sse2.CompareEqual(chunk3, comparisonMask);
                int matches3 = Sse2.MoveMask(equalMask3);
                return BitOperations.RotateRight((ulong)matches3 << 48 | (ulong)matches2 << 32 | (ulong)matches1 << 16 | (uint)matches0, (int)head);
            }")), "NETCOREAPP3_0_OR_GREATER")
        );
    }

    public void ModifyProject()
    {
        var ctzBody = ParseBody("assert(val != 0);return (uint) BitOperations.TrailingZeroCount(val);");
        var clzBody = ParseBody("assert(val != 0);return (uint) BitOperations.LeadingZeroCount(val);");
        var highBitBody = ParseBody("assert(val != 0);return (uint) BitOperations.Log2(val);");
        // 1.5.2
        if (projectBuilder.HasMethod("ZSTD_countTrailingZeros"))
        {
            ModifyMethod("ZSTD_countTrailingZeros", (builder, method) =>
            {
                builder.AddUsingDirective("System.Numerics");
                return method.WithBody(ctzBody);
            });

            ModifyMethod("FSE_ctz", (builder, method) =>
            {
                builder.AddUsingDirective("System.Numerics");
                return method.WithBody(ctzBody);
            });

            ModifyMethod("BIT_highbit32", (builder, method) =>
            {
                builder.AddUsingDirective("System.Numerics");
                return method.WithBody(highBitBody);
            });
        }
        else
        {
            // 1.5.3
            ModifyMethod("ZSTD_countTrailingZeros32", (builder, method) =>
            {
                builder.AddUsingDirective("System.Numerics");
                return method.WithBody(ctzBody);
            });

            ModifyMethod("ZSTD_countTrailingZeros64", (builder, method) =>
            {
                builder.AddUsingDirective("System.Numerics");
                return method.WithBody(ctzBody);
            });

            ModifyMethod("ZSTD_countLeadingZeros32", (builder, method) =>
            {
                builder.AddUsingDirective("System.Numerics");
                return method.WithBody(clzBody);
            });

            ModifyMethod("ZSTD_countLeadingZeros64", (builder, method) =>
            {
                builder.AddUsingDirective("System.Numerics");
                return method.WithBody(clzBody);
            });
        }

        ModifyMethod("ZSTD_highbit32", (builder, method) =>
        {
            builder.AddUsingDirective("System.Numerics");
            return method.WithBody(highBitBody);
        });

        ModifyMethod("ZSTD_NbCommonBytes", (builder, method) =>
        {
            builder.AddUsingDirective("System.Numerics");
            return method.WithBody(ParseBody(
                "            assert(val != 0);\r\n" +
                "            if (BitConverter.IsLittleEndian)\r\n" +
                "            {\r\n" +
                "                return MEM_64bits ? (uint)BitOperations.TrailingZeroCount(val) >> 3 : (uint)BitOperations.TrailingZeroCount((uint)val) >> 3;\r\n" +
                "            }\r\n\r\n" +
                "            return MEM_64bits ? (uint)BitOperations.LeadingZeroCount(val) >> 3 : (uint)BitOperations.LeadingZeroCount((uint)val) >> 3;"));
        });

        ModifyMethod("ZSTD_VecMask_next", (builder, method) =>
        {
            builder.AddUsingDirective("System.Numerics");
            return method.WithBody(ctzBody);
        });

        ModifyMethod("XXH_memcpy", (builder, method) =>
        {
            builder.AddUsingDirective("static ZstdSharp.UnsafeHelper");
            return method.WithBody(ParseBody("memcpy(dest, src, (uint)size);"));
        });

        ModifyMethod("ZSTD_copy16", (builder, method) =>
        {
            builder.AddUsingDirective("System.Runtime.Intrinsics.Arm", "System.Runtime.Intrinsics.X86");

            return method.WithBody(ParseBody(
                "\r\n#if NET5_0_OR_GREATER\r\n" +
                "            if (AdvSimd.IsSupported)\r\n" +
                "            {\r\n" +
                "                AdvSimd.Store((byte*) dst, AdvSimd.LoadVector128((byte*) src));\r\n" +
                "            } else\r\n" +
                "#endif\r\n" +
                "#if NETCOREAPP3_0_OR_GREATER\r\n" +
                "            if (Sse2.IsSupported)\r\n" +
                "            {\r\n" +
                "                Sse2.Store((byte*) dst, Sse2.LoadVector128((byte*) src));\r\n" +
                "            } else" +
                "\r\n" +
                "#endif\r\n" +
                "            {\r\n" +
                "                var v1 = System.Runtime.CompilerServices.Unsafe.ReadUnaligned<ulong>((ulong*)src);\r\n" +
                "                var v2 = System.Runtime.CompilerServices.Unsafe.ReadUnaligned<ulong>((ulong*)src + 1);\r\n" +
                "                System.Runtime.CompilerServices.Unsafe.WriteUnaligned((ulong*)dst, v1);\r\n" +
                "                System.Runtime.CompilerServices.Unsafe.WriteUnaligned((ulong*)dst + 1, v2);\r\n" +
                "            }"));
        });

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

        // Skip inits
        ModifyMethod("ZSTD_buildSequencesStatistics", (_, method) => AddSkipInit(method, "stats"));
        ModifyMethod("ZSTD_errorFrameSizeInfo", (_, method) => AddSkipInit(method, "frameSizeInfo"));
        ModifyMethod("ZSTD_get1BlockSummary", (_, method) => AddSkipInit(method, "bs"));
        // remove inlining
        ModifyMethod("ZSTD_encodeSequences_body",
            (_, method) => method
                .WithAttributeLists(SyntaxFactory.List(method.AttributeLists.Where(attributeList =>
                    !attributeList.Attributes.Any(attribute =>
                        attribute.ToString().Contains("MethodImplOptions.AggressiveInlining"))))));

        // switch to ifs
        ModifyMethod("ZSTD_hashPtr", (_, method) => ConvertMethodSwitchToIfs(method));
        // switch to ifs
        ModifyMethod("ZSTD_hashPtrSalted", (_, method) => ConvertMethodSwitchToIfs(method));

        ModifyVectorCode();

        if (projectBuilder.HasMethod("ZSTD_searchMax"))
        {
            // replace switches with ifs, remove unreachable branches
            ModifyMethod("ZSTD_searchMax",
                (_, method) => method
                    .WithBody(ParseBody(@"
            if (dictMode == ZSTD_dictMode_e.ZSTD_noDict)
            {
                if (searchMethod == searchMethod_e.search_rowHash)
                {
                    if (mls == 4)
                    {
                        if (rowLog == 4)
                            return ZSTD_RowFindBestMatch_noDict_4_4(ms, ip, iend, offsetPtr);
                        return rowLog == 5 ? ZSTD_RowFindBestMatch_noDict_4_5(ms, ip, iend, offsetPtr) : ZSTD_RowFindBestMatch_noDict_4_6(ms, ip, iend, offsetPtr);
                    }

                    if (mls == 5)
                    {
                        if (rowLog == 4)
                            return ZSTD_RowFindBestMatch_noDict_5_4(ms, ip, iend, offsetPtr);
                        return rowLog == 5 ? ZSTD_RowFindBestMatch_noDict_5_5(ms, ip, iend, offsetPtr) : ZSTD_RowFindBestMatch_noDict_5_6(ms, ip, iend, offsetPtr);
                    }

                    if (rowLog == 4)
                        return ZSTD_RowFindBestMatch_noDict_6_4(ms, ip, iend, offsetPtr);
                    return rowLog == 5 ? ZSTD_RowFindBestMatch_noDict_6_5(ms, ip, iend, offsetPtr) : ZSTD_RowFindBestMatch_noDict_6_6(ms, ip, iend, offsetPtr);
                }

                if (searchMethod == searchMethod_e.search_hashChain)
                {
                    if (mls == 4)
                        return ZSTD_HcFindBestMatch_noDict_4(ms, ip, iend, offsetPtr);
                    return mls == 5 ? ZSTD_HcFindBestMatch_noDict_5(ms, ip, iend, offsetPtr) : ZSTD_HcFindBestMatch_noDict_6(ms, ip, iend, offsetPtr);
                }

                // searchMethod_e.search_binaryTree
                if (mls == 4)
                    return ZSTD_BtFindBestMatch_noDict_4(ms, ip, iend, offsetPtr);
                return mls == 5 ? ZSTD_BtFindBestMatch_noDict_5(ms, ip, iend, offsetPtr) : ZSTD_BtFindBestMatch_noDict_6(ms, ip, iend, offsetPtr);
            }

            if (dictMode == ZSTD_dictMode_e.ZSTD_extDict)
            {
                if (searchMethod == searchMethod_e.search_rowHash)
                {
                    if (mls == 4)
                    {
                        if (rowLog == 4)
                            return ZSTD_RowFindBestMatch_extDict_4_4(ms, ip, iend, offsetPtr);
                        if (rowLog == 5)
                            return ZSTD_RowFindBestMatch_extDict_4_5(ms, ip, iend, offsetPtr);
                        return ZSTD_RowFindBestMatch_extDict_4_6(ms, ip, iend, offsetPtr);
                    }

                    if (mls == 5)
                    {
                        if (rowLog == 4)
                            return ZSTD_RowFindBestMatch_extDict_5_4(ms, ip, iend, offsetPtr);
                        if (rowLog == 5)
                            return ZSTD_RowFindBestMatch_extDict_5_5(ms, ip, iend, offsetPtr);
                        return ZSTD_RowFindBestMatch_extDict_5_6(ms, ip, iend, offsetPtr);
                    }

                    if (mls == 6)
                    {
                        if (rowLog == 4)
                            return ZSTD_RowFindBestMatch_extDict_6_4(ms, ip, iend, offsetPtr);
                        if (rowLog == 5)
                            return ZSTD_RowFindBestMatch_extDict_6_5(ms, ip, iend, offsetPtr);
                        return ZSTD_RowFindBestMatch_extDict_6_6(ms, ip, iend, offsetPtr);
                    }
                }

                if (searchMethod == searchMethod_e.search_hashChain)
                {
                    if (mls == 4)
                        return ZSTD_HcFindBestMatch_extDict_4(ms, ip, iend, offsetPtr);
                    if (mls == 5)
                        return ZSTD_HcFindBestMatch_extDict_5(ms, ip, iend, offsetPtr);
                    return ZSTD_HcFindBestMatch_extDict_6(ms, ip, iend, offsetPtr);
                }

                // searchMethod_e.search_binaryTree
                if (mls == 4)
                    return ZSTD_BtFindBestMatch_extDict_4(ms, ip, iend, offsetPtr);
                if (mls == 5)
                    return ZSTD_BtFindBestMatch_extDict_5(ms, ip, iend, offsetPtr);
                return ZSTD_BtFindBestMatch_extDict_6(ms, ip, iend, offsetPtr);
            }

            if (dictMode == ZSTD_dictMode_e.ZSTD_dictMatchState)
            {
                if (searchMethod == searchMethod_e.search_rowHash)
                {
                    if (mls == 4)
                    {
                        if (rowLog == 4)
                            return ZSTD_RowFindBestMatch_dictMatchState_4_4(ms, ip, iend, offsetPtr);
                        if (rowLog == 5)
                            return ZSTD_RowFindBestMatch_dictMatchState_4_5(ms, ip, iend, offsetPtr);
                        return ZSTD_RowFindBestMatch_dictMatchState_4_6(ms, ip, iend, offsetPtr);
                    }

                    if (mls == 5)
                    {
                        if (rowLog == 4)
                            return ZSTD_RowFindBestMatch_dictMatchState_5_4(ms, ip, iend, offsetPtr);
                        if (rowLog == 5)
                            return ZSTD_RowFindBestMatch_dictMatchState_5_5(ms, ip, iend, offsetPtr);
                        return ZSTD_RowFindBestMatch_dictMatchState_5_6(ms, ip, iend, offsetPtr);
                    }

                    if (mls == 6)
                    {
                        if (rowLog == 4)
                            return ZSTD_RowFindBestMatch_dictMatchState_6_4(ms, ip, iend, offsetPtr);
                        if (rowLog == 5)
                            return ZSTD_RowFindBestMatch_dictMatchState_6_5(ms, ip, iend, offsetPtr);
                        return ZSTD_RowFindBestMatch_dictMatchState_6_6(ms, ip, iend, offsetPtr);
                    }
                }

                if (searchMethod == searchMethod_e.search_hashChain)
                {
                    if (mls == 4)
                        return ZSTD_HcFindBestMatch_dictMatchState_4(ms, ip, iend, offsetPtr);
                    if (mls == 5)
                        return ZSTD_HcFindBestMatch_dictMatchState_5(ms, ip, iend, offsetPtr);
                    return ZSTD_HcFindBestMatch_dictMatchState_6(ms, ip, iend, offsetPtr);
                }

                // search_binaryTree
                if (mls == 4)
                    return ZSTD_BtFindBestMatch_dictMatchState_4(ms, ip, iend, offsetPtr);
                if (mls == 5)
                    return ZSTD_BtFindBestMatch_dictMatchState_5(ms, ip, iend, offsetPtr);
                return ZSTD_BtFindBestMatch_dictMatchState_6(ms, ip, iend, offsetPtr);
            }

            if (searchMethod == searchMethod_e.search_rowHash)
            {
                if (mls == 4)
                {
                    if (rowLog == 4)
                        return ZSTD_RowFindBestMatch_dedicatedDictSearch_4_4(ms, ip, iend, offsetPtr);
                    if (rowLog == 5)
                        return ZSTD_RowFindBestMatch_dedicatedDictSearch_4_5(ms, ip, iend, offsetPtr);
                    return ZSTD_RowFindBestMatch_dedicatedDictSearch_4_6(ms, ip, iend, offsetPtr);
                }

                if (mls == 5)
                {
                    if (rowLog == 4)
                        return ZSTD_RowFindBestMatch_dedicatedDictSearch_5_4(ms, ip, iend, offsetPtr);
                    if (rowLog == 5)
                        return ZSTD_RowFindBestMatch_dedicatedDictSearch_5_5(ms, ip, iend, offsetPtr);
                    return ZSTD_RowFindBestMatch_dedicatedDictSearch_5_6(ms, ip, iend, offsetPtr);
                }

                if (mls == 6)
                {
                    if (rowLog == 4)
                        return ZSTD_RowFindBestMatch_dedicatedDictSearch_6_4(ms, ip, iend, offsetPtr);
                    if (rowLog == 5)
                        return ZSTD_RowFindBestMatch_dedicatedDictSearch_6_5(ms, ip, iend, offsetPtr);
                    return ZSTD_RowFindBestMatch_dedicatedDictSearch_6_6(ms, ip, iend, offsetPtr);
                }
            }

            if (searchMethod == searchMethod_e.search_hashChain)
            {
                if (mls == 4)
                    return ZSTD_HcFindBestMatch_dedicatedDictSearch_4(ms, ip, iend, offsetPtr);
                if (mls == 5)
                    return ZSTD_HcFindBestMatch_dedicatedDictSearch_5(ms, ip, iend, offsetPtr);
                return ZSTD_HcFindBestMatch_dedicatedDictSearch_6(ms, ip, iend, offsetPtr);
            }

            // searchMethod_e.search_binaryTree
            if (mls == 4)
                return ZSTD_BtFindBestMatch_dedicatedDictSearch_4(ms, ip, iend, offsetPtr);
            if (mls == 5)
                return ZSTD_BtFindBestMatch_dedicatedDictSearch_5(ms, ip, iend, offsetPtr);
            return ZSTD_BtFindBestMatch_dedicatedDictSearch_6(ms, ip, iend, offsetPtr);")));
        }

        // v1.5.4
        // improve fast c loop functions
        ModifyFastCLoop(FastCLoopMethod.Decompress4X1);
        ModifyFastCLoop(FastCLoopMethod.Decompress4X2);
        // Guard full assert blocks with DEBUG condition
        GuardAsserts();
        // x = new T(); x.f = ...; -> x = new T { f = ... };
        ProcessStructInitialization();

        // improve compression/decompression
        var refMethods = new RefMethods(projectBuilder, reporter);
        refMethods.Run();
        new ImproveDecompressSequences(projectBuilder, reporter, refMethods.Methods).Run();
        new ImproveFseCompress(projectBuilder, refMethods.Methods).Run();
        new ImproveFseDecompress(projectBuilder, refMethods.Methods).Run();
        new ImproveHufCompress(projectBuilder, refMethods.Methods).Run();
        new ImproveCompressSequences(projectBuilder, refMethods.Methods).Run();
    }

    private void ProcessStructInitialization()
    {
        var structInitialization = new StructInitialization();
        foreach (var methodName in projectBuilder.GetMethods())
        {
            ModifyMethod(methodName, (_, method) => structInitialization.Process(method));
        }
    }

    private void ModifyFastCLoop(FastCLoopMethod fastCLoopMethod)
    {
        var methodName = fastCLoopMethod switch
        {
            FastCLoopMethod.Decompress4X1 => "HUF_decompress4X1_usingDTable_internal_fast_c_loop",
            FastCLoopMethod.Decompress4X2 => "HUF_decompress4X2_usingDTable_internal_fast_c_loop",
            _ => throw new ArgumentOutOfRangeException(nameof(fastCLoopMethod), fastCLoopMethod, null)
        };

        ModifyMethod(methodName,
            (_, method) => new FastCLoopModifier(fastCLoopMethod, reporter).RewriteMethod(method));
    }
}

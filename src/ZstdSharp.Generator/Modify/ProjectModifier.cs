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

internal class ProjectModifier
{
    private readonly ProjectBuilder _projectBuilder;
    private readonly IReporter _reporter;

    public ProjectModifier(ProjectBuilder projectBuilder, IReporter reporter)
    {
        _projectBuilder = projectBuilder;
        _reporter = reporter;
    }

    private void ModifyMethod(string name,
        Func<FileBuilder, MethodDeclarationSyntax, MethodDeclarationSyntax?> modifier)
    {
        if (!_projectBuilder.ModifyMethod(name, modifier))
        {
            _reporter.Report(DiagnosticLevel.Error, $"No method {name}");
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
                    _reporter.Report(DiagnosticLevel.Error, "Failed to convert switch to ifs");
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

    internal static MethodDeclarationSyntax AddSkipInit(MethodDeclarationSyntax method, string varName)
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

    private void GuardAsserts()
    {
        foreach (var methodName in _projectBuilder.GetMethods())
        {
            ModifyMethod(methodName, (builder, method) => new AssertsRewriter(builder).RewriteMethod(method));
        }
    }

    public void ModifyProject()
    {
        var ctzBody = ParseBody("assert(val != 0);return (uint) BitOperations.TrailingZeroCount(val);");
        var clzBody = ParseBody("assert(val != 0);return (uint) BitOperations.LeadingZeroCount(val);");
        var highBitBody = ParseBody("assert(val != 0);return (uint) BitOperations.Log2(val);");
        // 1.5.2
        if (_projectBuilder.HasMethod("ZSTD_countTrailingZeros"))
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
                "#if NET5_0_OR_GREATER\r\n" +
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

        // Skip inits
        ModifyMethod("ZSTD_buildSequencesStatistics", (_, method) => AddSkipInit(method, "stats"));
        ModifyMethod("ZSTD_errorFrameSizeInfo", (_, method) => AddSkipInit(method, "frameSizeInfo"));
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

        // arm/sse2/soft versions
        ModifyMethod("ZSTD_row_getMatchMask", (builder, method) =>
        {
            string? headParameterName;
            if (method.ParameterList.Parameters.Any(p => p.Identifier.ToString() == "head"))
            {
                headParameterName = "head";
            }
            else if (method.ParameterList.Parameters.Any(p => p.Identifier.ToString() == "headGrouped"))
            {
                headParameterName = "headGrouped";
            }
            else
            {
                _reporter.Report(DiagnosticLevel.Error, "No head parameter");
                return method;
            }

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
                $"            /* This NEON path only works for little endian - otherwise use SWAR below */\r\n" +
                $"            if (AdvSimd.IsSupported && BitConverter.IsLittleEndian)\r\n" +
                $"            {{\r\n" +
                $"                if (rowEntries == 16)\r\n" +
                $"                {{\r\n" +
                $"                    Vector128<byte> chunk = AdvSimd.LoadVector128(src);\r\n" +
                $"                    Vector128<UInt16> equalMask = AdvSimd.CompareEqual(chunk, AdvSimd.DuplicateToVector128(tag)).As<byte, UInt16>();\r\n" +
                $"                    Vector128<UInt16> t0 = AdvSimd.ShiftLeftLogical(equalMask, 7);\r\n" +
                $"                    Vector128<UInt32> t1 = AdvSimd.ShiftRightAndInsert(t0, t0, 14).As<UInt16, UInt32>();\r\n" +
                $"                    Vector128<UInt64> t2 = AdvSimd.ShiftRightLogical(t1, 14).As<UInt32, UInt64>();\r\n" +
                $"                    Vector128<byte> t3 = AdvSimd.ShiftRightLogicalAdd(t2, t2, 28).As<UInt64, byte>();\r\n" +
                $"                    ushort hi = AdvSimd.Extract(t3, 8);\r\n" +
                $"                    ushort lo = AdvSimd.Extract(t3, 0);\r\n" +
                $"                    return BitOperations.RotateRight((ushort)((hi << 8) | lo), (int){headParameterName});\r\n" +
                $"                }}\r\n" +
                $"                else if (rowEntries == 32)\r\n" +
                $"                {{\r\n" +
                $"                    // todo, there is no vld2q_u16 in c#\r\n" +
                $"                }}\r\n" +
                $"                else\r\n" +
                $"                {{ /* rowEntries == 64 */\r\n" +
                $"                    // todo, there is no vld4q_u8 in c#\r\n" +
                $"                }}\r\n" +
                $"            }}\r\n");
            builder.AddUsingDirective("System.Runtime.Intrinsics.Arm");
            var armImplementationStatements = WrapWithIfDefined(armImplementation.Statements, "NET5_0_OR_GREATER");
            statements.AddRange(armImplementationStatements);

            // soft implementation
            var softImplementation = ParseBody(
                $"                nuint chunkSize = (nuint)sizeof(nuint);\r\n" +
                $"                nuint shiftAmount = chunkSize * 8 - chunkSize;\r\n" +
                $"                nuint xFF = ~(nuint)0;\r\n" +
                $"                nuint x01 = xFF / 0xFF;\r\n" +
                $"                nuint x80 = x01 << 7;\r\n" +
                $"                nuint splatChar = tag * x01;\r\n" +
                $"                ulong matches = 0;\r\n" +
                $"                int i = (int)(rowEntries - chunkSize);\r\n" +
                $"                assert(sizeof(nuint) == 4 || sizeof(nuint) == 8);\r\n" +
                $"                if (BitConverter.IsLittleEndian)\r\n" +
                $"                {{\r\n" +
                $"                    nuint extractMagic = xFF / 0x7F >> (int)chunkSize;\r\n" +
                $"                    do\r\n" +
                $"                    {{\r\n" +
                $"                        nuint chunk = MEM_readST(&src[i]);\r\n" +
                $"                        chunk ^= splatChar;\r\n" +
                $"                        chunk = ((chunk | x80) - x01 | chunk) & x80;\r\n" +
                $"                        matches <<= (int)chunkSize;\r\n" +
                $"                        matches |= (ulong)(chunk * extractMagic >> (int)shiftAmount);\r\n" +
                $"                        i -= (int)chunkSize;\r\n" +
                $"                    }}\r\n" +
                $"                    while (i >= 0);\r\n" +
                $"                }}\r\n" +
                $"                else\r\n" +
                $"                {{\r\n" +
                $"                    nuint msb = xFF ^ xFF >> 1;\r\n" +
                $"                    nuint extractMagic = msb / 0x1FF | msb;\r\n" +
                $"                    do\r\n" +
                $"                    {{\r\n" +
                $"                        nuint chunk = MEM_readST(&src[i]);\r\n" +
                $"                        chunk ^= splatChar;\r\n" +
                $"                        chunk = ((chunk | x80) - x01 | chunk) & x80;\r\n" +
                $"                        matches <<= (int)chunkSize;\r\n" +
                $"                        matches |= (ulong)((chunk >> 7) * extractMagic >> (int)shiftAmount);\r\n" +
                $"                        i -= (int)chunkSize;\r\n" +
                $"                    }}\r\n" +
                $"                    while (i >= 0);\r\n" +
                $"                }}\r\n\r\n" +
                $"                matches = ~matches;\r\n" +
                $"                if (rowEntries == 16)\r\n" +
                $"                {{\r\n" +
                $"                    return BitOperations.RotateRight((ushort)matches, (int){headParameterName});\r\n" +
                $"                }}\r\n" +
                $"                else if (rowEntries == 32)\r\n" +
                $"                {{\r\n" +
                $"                    return BitOperations.RotateRight((uint)matches, (int){headParameterName});\r\n" +
                $"                }}\r\n" +
                $"                else\r\n" +
                $"                {{\r\n" +
                $"                    return BitOperations.RotateRight((ulong)matches, (int){headParameterName});\r\n" +
                $"                }}");
            builder.AddUsingDirective("System", "System.Numerics");
            statements.Add(softImplementation);

            return method.WithBody(body
                .ReplaceNode(returnStatement, statements));
        });

        if (_projectBuilder.HasMethod("ZSTD_searchMax"))
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

        // improve decompression
        new ImproveDecompressSequences(_projectBuilder, _reporter).Run();
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
            (_, method) => new FastCLoopModifier(fastCLoopMethod, _reporter).RewriteMethod(method));
    }
}

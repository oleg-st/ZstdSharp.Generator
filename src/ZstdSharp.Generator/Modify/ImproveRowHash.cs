using System;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace ZstdSharp.Generator.Modify;

internal sealed class ImproveRowHash(string method, ExpressionSyntax initialShift) : CSharpSyntaxRewriter
{
    private const string Match = "ZSTD_RowFindBestMatch", Fill = "ZSTD_row_fillHashCache";
    private const string Update = "ZSTD_row_update_internal", Range = "ZSTD_row_update_internalImpl";
    private const string Next = "ZSTD_row_nextCachedHash", Hash = "ZSTD_row_hashWithShift", Index = "ZSTD_row_nextIndex";

    public static void Run(ProjectBuilder project)
    {
        if (!project.TryGetMethod(Match, out var file, out _))
            return;

        MethodDeclarationSyntax Source(string name) => project.TryGetMethod(name, out _, out var body)
            ? body : throw new InvalidOperationException($"Missing row hash prerequisite {name}.");

        var hashCall = Source(Fill).DescendantNodes().OfType<InvocationExpressionSyntax>().Single(c => Called(c) == "ZSTD_hashPtrSalted");
        if (TreeHelper.GetInnerExpression(hashCall.ArgumentList.Arguments[1].Expression) is not BinaryExpressionSyntax width
            || !width.IsKind(SyntaxKind.AddExpression) || !TreeHelper.GetValue(width.Right, out var tagBits))
            throw new InvalidOperationException("Expected row hash width plus tag bits.");
        var initial = ParseExpression($"{64 - Convert.ToInt32(tagBits)} - (int)ms->rowHashLog");

        // Reuse the load and multiplier from the common hash functions.
        string HashCase(int n)
        {
            var returned = TreeHelper.GetInnerExpression(Source($"ZSTD_hash{n}").DescendantNodes()
                .OfType<ReturnStatementSyntax>().Single().Expression!);
            if (n != 4 && returned is CastExpressionSyntax cast && cast.Type.ToString() == "nuint")
                returned = TreeHelper.GetInnerExpression(cast.Expression);
            var product = returned.DescendantNodes().OfType<BinaryExpressionSyntax>().Single(b => b.IsKind(SyntaxKind.MultiplyExpression));
            int bits = n == 4 ? 32 : 64;
            if (!Matches(returned, $"({product.NormalizeWhitespace()} ^ s) >> (int)({bits} - h)"))
                throw new InvalidOperationException($"Unexpected multiply, salt or shift in ZSTD_hash{n}.");
            var read = Source($"ZSTD_hash{n}PtrS").DescendantNodes().OfType<InvocationExpressionSyntax>()
                .Single(c => Called(c) is "MEM_readLE32" or "MEM_readLE64");
            string prime = product.Right.ToString(), load = $"{read.Expression}(src)";
            if (n == 4)
                return $"if (mls == 4) return ({load} * {prime} ^ (uint)salt) >> rowShift;";
            return $"if (mls == {n}) return (uint)(({load} * {prime} ^ salt) >> rowShift);";
        }

        var helper = (MethodDeclarationSyntax)ParseMemberDeclaration($$"""
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            private static uint {{Hash}}(byte* src, int rowShift, uint mls, ulong salt)
            {
                assert((uint)(64 - rowShift) <= 32);
                // C# masks a 32-bit shift count to its low five bits.
                {{HashCase(4)}}
                {{HashCase(5)}}
                {{HashCase(6)}}
                return (uint)ZSTD_hashPtrSalted(src, (uint)(64 - rowShift), mls, salt);
            }
            """)!;
        file.AddMethodWithName(helper, Hash);

        string[] targets = [Match, Fill, Update, Range, Next, Index, "ZSTD_row_update",
            "ZSTD_compressBlock_lazy_generic", "ZSTD_compressBlock_lazy_extDict_generic"];
        foreach (var target in targets)
            if (!project.ModifyMethod(target, (_, node) => new ImproveRowHash(target, initial).Rewrite(node)))
                throw new InvalidOperationException($"Missing row hash caller {target}.");
    }

    private static string? Called(InvocationExpressionSyntax call) => (call.Expression as IdentifierNameSyntax)?.Identifier.ValueText;

    private static bool Matches(ExpressionSyntax actual, string expected)
    {
        // Clang's generated AST may represent C# keywords as identifier nodes.
        // Parse both expressions before comparing operators and ignoring redundant parentheses.
        static SyntaxNode Shape(string source)
        {
            var expression = ParseExpression(source);
            return expression.ReplaceNodes(expression.DescendantNodesAndSelf().OfType<ParenthesizedExpressionSyntax>(),
                (_, rewritten) => rewritten.Expression);
        }
        return AreEquivalent(Shape(actual.NormalizeWhitespace().ToFullString()), Shape(expected));
    }

    private BlockSyntax UpdateRowsBeforeSearch(BlockSyntax body)
    {
        var update = body.DescendantNodes().OfType<InvocationExpressionSyntax>().Single(call => Called(call) == Update);
        if (update.Parent is not ExpressionStatementSyntax statement || statement.Parent is not BlockSyntax cached
            || cached.Parent is not IfStatementSyntax branch || branch.Parent != body || branch.Statement != cached
            || cached.Statements.First() != statement || !Matches(branch.Condition, "ms->lazySkipping == 0"))
            throw new InvalidOperationException("Expected the row update at the start of the cached search.");

        ExpressionSyntax InitialValue(string name) => body.Statements.OfType<LocalDeclarationStatementSyntax>()
            .SelectMany(local => local.Declaration.Variables).Single(v => v.Identifier.ValueText == name).Initializer!.Value;
        var mask = InitialValue("rowMask");
        var entries = InitialValue("rowEntries");
        if (!Matches(mask, "rowEntries - 1") || !Matches(entries, "1U << (int)rowLog"))
            throw new InvalidOperationException("Expected a row mask derived from rowLog.");
        var earlyMask = mask.ReplaceNode(mask.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>().Single(),
            ParenthesizedExpression(entries));
        var maskArgument = update.ArgumentList.Arguments.Single(arg => Matches(arg.Expression, "rowMask"));

        // Update table entries before loading search bounds so fewer locals are live across the loop.
        var useCache = IdentifierName("useCache");
        var earlyUpdate = statement.ReplaceNode(maskArgument.Expression, earlyMask);
        var remaining = branch.WithCondition(useCache).WithStatement(cached.WithStatements(cached.Statements.RemoveAt(0)));
        return body.WithStatements(body.Statements.Replace(branch, remaining).InsertRange(0, [
            ParseStatement($"int rowShift = {initialShift};"),
            ParseStatement($"bool useCache = {branch.Condition.NormalizeWhitespace()};"),
            IfStatement(useCache, earlyUpdate)]));
    }

    private MethodDeclarationSyntax Rewrite(MethodDeclarationSyntax node)
    {
        var body = (BlockSyntax)Visit(node.Body!)!;
        var shift = Parameter(Identifier("rowShift")).WithType(ParseTypeName("int"));
        if (method is Update or Fill or Range)
            node = node.AddParameterListParameters(shift);
        if (method == Next)
            node = node.ReplaceNode(node.ParameterList.Parameters.Single(p => p.Identifier.ValueText == "hashLog"), shift);
        if (method == Match)
            body = UpdateRowsBeforeSearch(body);
        if (method == Range)
        {
            var loop = body.Statements.OfType<ForStatementSyntax>().Single();
            body = body.WithStatements(body.Statements.InsertRange(body.Statements.IndexOf(loop),
                [ParseStatement("uint* cache = ms->hashCache;"), ParseStatement("ulong salt = ms->hashSalt;")]));
        }
        if (method == Index)
        {
            var adjust = body.DescendantNodes().OfType<AssignmentExpressionSyntax>().Single(a =>
                a.Left is IdentifierNameSyntax { Identifier.ValueText: "next" } && a.DescendantNodes().OfType<ConditionalExpressionSyntax>().Any());
            if (!Matches(adjust, "next += next == 0 ? rowMask : 0"))
                throw new InvalidOperationException("Unexpected reserved-slot adjustment in ZSTD_row_nextIndex.");
            // A zero next index skips the reserved first slot. rowMask is at most 63.
            body = body.ReplaceNode(adjust, ParseExpression("next |= (uint)((int)(next - 1) >> 31) & rowMask").WithTriviaFrom(adjust));
            if (body.Statements is not [LocalDeclarationStatementSyntax { Declaration.Variables: [{ Initializer.Value: var init }] },
                _, ExpressionStatementSyntax { Expression: var store }, ReturnStatementSyntax { Expression: IdentifierNameSyntax { Identifier.ValueText: "next" } }]
                || !Matches(init, "(uint)(*tagRow - 1) & rowMask") || !Matches(store, "*tagRow = (byte)next"))
                throw new InvalidOperationException("Unexpected row head update in ZSTD_row_nextIndex.");

            // Each nibble maps the current head to the preceding nonzero slot. Heads 0 and 1 wrap to slot 15.
            var lookup = ParseStatement("""
                if (Bmi2.X64.IsSupported && rowMask == 15)
                {
                    uint next4 = (uint)(0xEDCBA987654321FFUL >> (*tagRow << 2)) & 15;
                    *tagRow = (byte)next4;
                    return next4;
                }
                """);
            body = body.WithStatements(body.Statements.Insert(0, ProjectModifier.WrapWithIfDefined(lookup, "NETCOREAPP3_0_OR_GREATER")));
        }
        return node.WithBody(body);
    }

    public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
    {
        if (node.Declaration.Variables is not [var variable]) return base.VisitLocalDeclarationStatement(node);
        var name = variable.Identifier.ValueText;
        if (name == "hashLog" && method is Match or Fill or Range) return null;
        if (method == Range && name is "relRow" or "slot")
        {
            if (name == "relRow")
            {
                var value = (BinaryExpressionSyntax)TreeHelper.GetInnerExpression(variable.Initializer!.Value);
                if (!value.IsKind(SyntaxKind.LeftShiftExpression)) throw new InvalidOperationException("Expected row-offset shift.");
                node = node.ReplaceNode(value, value.WithLeft(CastExpression(ParseTypeName("nuint"), ParenthesizedExpression(value.Left))));
            }
            return node.WithDeclaration(node.Declaration.WithType(ParseTypeName("nuint")));
        }
        return base.VisitLocalDeclarationStatement(node);
    }

    public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
    {
        if (method == Range && node.Expression is IdentifierNameSyntax { Identifier.ValueText: "ms" })
        {
            if (node.Name.Identifier.ValueText == "hashCache") return IdentifierName("cache");
            if (node.Name.Identifier.ValueText == "hashSalt") return IdentifierName("salt");
        }
        return base.VisitMemberAccessExpression(node);
    }

    public override SyntaxNode? VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        var call = (InvocationExpressionSyntax)base.VisitInvocationExpression(node)!;
        var args = call.ArgumentList.Arguments;
        switch (Called(call))
        {
            case Fill or Update or Range:
                var shift = method is Match or Update ? IdentifierName("rowShift") : initialShift;
                return call.WithArgumentList(call.ArgumentList.AddArguments(Argument(shift)));
            case Next:
                return call.WithArgumentList(call.ArgumentList.WithArguments(args.Replace(args[5], Argument(IdentifierName("rowShift")))));
            case "ZSTD_hashPtrSalted":
                if (node.Ancestors().OfType<InvocationExpressionSyntax>().Any(c => Called(c) == "assert"))
                {
                    // Keep the upstream hash as an independent Debug cross-check of the ring.
                    var name = args[1].Expression.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>().Single(i => i.Identifier.ValueText == "hashLog");
                    return call.ReplaceNode(name, ParseExpression("ms->rowHashLog"));
                }
                return call.WithExpression(IdentifierName(Hash)).WithArgumentList(
                    call.ArgumentList.WithArguments(args.Replace(args[1], Argument(IdentifierName("rowShift")))));
            default: return call;
        }
    }
}

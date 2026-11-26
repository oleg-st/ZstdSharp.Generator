using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace ZstdSharp.Generator.Modify;

internal static class ImproveRowSearch
{
    public static void Run(ProjectBuilder project)
    {
        if (!project.HasMethod("ZSTD_RowFindBestMatch"))
            return;
        if (!project.ModifyMethod("ZSTD_row_update_internalImpl", (_, method) => ShareUpdateIndex(method)))
            throw new InvalidOperationException("Cannot find the row table updater.");
        project.ModifyMethod("ZSTD_RowFindBestMatch", (_, method) => ShortenScoringLocals(method));
        ImproveRowHash.Run(project);
        project.ModifyMethod("ZSTD_RowFindBestMatch", UseFixedMatchBuffer);
    }

    private static MethodDeclarationSyntax UseFixedMatchBuffer(FileBuilder file, MethodDeclarationSyntax method)
    {
        var body = method.Body!;
        var buffers = body.DescendantNodes().OfType<LocalDeclarationStatementSyntax>()
            .Where(s => LocalName(s) == "matchBuffer").ToArray();
        var expected = ParseStatement("uint* matchBuffer = stackalloc uint[64];");
        if (buffers.Length != 2 || buffers.Any(s => !AreEquivalent(ParseStatement(s.NormalizeWhitespace().ToString()), expected)))
            throw new InvalidOperationException("Expected 64-entry prefix and dictionary row match buffers.");

        // Each scan visits at most one row. The dictionary scan starts after prefix scoring finishes.
        // The fixed buffers keep a constant stack frame and each specialized search keeps only its row size.
        var locals = new List<StatementSyntax>();
        foreach (int size in new[] { 16, 32, 64 })
        {
            file.AddMethodsMember(ParseMemberDeclaration($"private struct RowMatchBuffer{size} {{ public fixed uint Indices[{size}]; }}")!);
            locals.Add(ParseStatement($"RowMatchBuffer{size} buffer{size};"));
        }
        locals.Add(ParseStatement("uint* matchBuffer = rowLog == 4 ? buffer16.Indices : rowLog == 5 ? buffer32.Indices : buffer64.Indices;"));
        body = body.RemoveNodes(buffers, SyntaxRemoveOptions.KeepNoTrivia)!;
        return method.WithBody(body.WithStatements(body.Statements.InsertRange(0, locals)));
    }

    private static string? LocalName(StatementSyntax statement) =>
        statement is LocalDeclarationStatementSyntax { Declaration.Variables: [var variable] }
            ? variable.Identifier.ValueText : null;

    private static MethodDeclarationSyntax ShareUpdateIndex(MethodDeclarationSyntax method)
    {
        var loop = method.Body!.DescendantNodes().OfType<ForStatementSyntax>().Single();
        var body = (BlockSyntax)loop.Statement;
        var row = body.Statements.Single(s => LocalName(s) == "row");
        var writes = body.Statements.OfType<ExpressionStatementSyntax>().Where(s =>
            s.Expression is AssignmentExpressionSyntax { Left: ElementAccessExpressionSyntax e }
            && e.Expression is IdentifierNameSyntax id && id.Identifier.ValueText is "row" or "tagRow").ToArray();
        if (writes.Length != 2 || !writes.All(s =>
            ((ElementAccessExpressionSyntax)((AssignmentExpressionSyntax)s.Expression).Left).ArgumentList.Arguments
                is [{ Expression: IdentifierNameSyntax { Identifier.ValueText: "pos" } }]))
            throw new InvalidOperationException("Expected tagRow[pos] and row[pos] stores.");

        var first = body.Statements.IndexOf(writes[0]);
        if (first + 1 != body.Statements.IndexOf(writes[1]))
            throw new InvalidOperationException("Row stores must be adjacent.");
        // relRow has rowLog low zero bits; pos fits in those bits. Addition cannot overflow
        // uint for any supported row width, even when the end of the hash table is used.
        var statements = new List<StatementSyntax>();
        foreach (var statement in body.Statements)
        {
            if (statement == row)
                continue;
            if (statement == writes[0])
                statements.Add(ParseStatement("uint slot = relRow + pos;"));
            if (statement == writes[0] || statement == writes[1])
            {
                var write = (AssignmentExpressionSyntax)((ExpressionStatementSyntax)statement).Expression;
                var address = (ElementAccessExpressionSyntax)write.Left;
                var table = ((IdentifierNameSyntax)address.Expression).Identifier.ValueText == "tagRow" ? "tagTable" : "hashTable";
                statements.Add(statement.ReplaceNode(address, ParseExpression(table + "[slot]")));
            }
            else
                statements.Add(statement);
        }
        return method.ReplaceNode(body, body.WithStatements(List(statements)));
    }

    private static MethodDeclarationSyntax ShortenScoringLocals(MethodDeclarationSyntax method)
    {
        var body = method.Body!;
        var ml = (LocalDeclarationStatementSyntax)body.Statements.Single(s => LocalName(s) == "ml");
        var initial = ml.Declaration.Variables.Single();
        var loops = body.DescendantNodes().OfType<ForStatementSyntax>().Where(loop =>
            loop.Statement is BlockSyntax score && score.Statements.Any(s => LocalName(s) == "currentMl")).ToArray();
        if (loops.Length != 2)
            throw new InvalidOperationException("Expected prefix and attached-dictionary candidate loops.");

        var scopes = loops.Select(loop => (BlockSyntax)loop.Parent!).ToArray();
        // Prefix scoring always executes before either dictionary search. Declare ml at
        // method scope for that handoff, but initialize it only after the prefix row is filled.
        var edits = scopes.Select((scope, i) => RetargetScoring(scope, loops[i], i == 0 ? initial.Initializer!.Value : null)).ToArray();
        var changed = body.ReplaceNodes(new SyntaxNode[] { ml, scopes[0], scopes[1] }, (old, _) =>
            old == ml ? ml.ReplaceNode(initial, initial.WithInitializer(null)) : old == scopes[0] ? edits[0] : edits[1]);
        return method.WithBody(changed);
    }

    private static BlockSyntax RetargetScoring(BlockSyntax scope, ForStatementSyntax loop, ExpressionSyntax? initialLength)
    {
        var score = (BlockSyntax)loop.Statement;
        var measured = (LocalDeclarationStatementSyntax)score.Statements.Single(s => LocalName(s) == "currentMl");
        var value = measured.Declaration.Variables.Single();
        if (value.Initializer == null || !TreeHelper.GetValue(value.Initializer.Value, out var zero, null)
            || Convert.ToUInt64(zero) != 0
            || score.Statements.Last() is not IfStatementSyntax improvement
            || TreeHelper.GetInnerExpression(improvement.Condition) is not BinaryExpressionSyntax check
            || !check.IsKind(SyntaxKind.GreaterThanExpression))
            throw new InvalidOperationException($"Unexpected row candidate scoring shape: {value.Initializer}; {score.Statements.Last()}");

        var probes = score.DescendantNodes().OfType<IfStatementSyntax>().Where(t =>
            t.Else == null && TreeHelper.GetInnerExpression(t.Condition) is BinaryExpressionSyntax test
            && test.IsKind(SyntaxKind.EqualsExpression)
            && t.Statement is ExpressionStatementSyntax { Expression: AssignmentExpressionSyntax assignment }
            && assignment.Left is IdentifierNameSyntax { Identifier.ValueText: "currentMl" }).ToArray();
        if (probes.Length != (initialLength == null ? 1 : 2))
            throw new InvalidOperationException("Expected the four-byte equality checks for prefix, external and attached dictionaries.");

        // Each true branch assigns the measured length. A failed first word cannot
        // improve ml (initially 3, subsequently only increased), so skip the candidate.
        // Both external/attached dictionary branches keep their original two-segment count.
        var blocks = probes.Select(t => (BlockSyntax)t.Parent!).ToArray();
        if (blocks.Any(b => !probes.Contains(b.Statements.Last())))
            throw new InvalidOperationException("Unexpected side effects after the candidate length probe.");
        var rewritten = score.ReplaceNodes(new SyntaxNode[] { measured }.Concat(blocks), (old, _) =>
        {
            if (old == measured)
                return measured.ReplaceNode(value, value.WithInitializer(null));
            var block = (BlockSyntax)old;
            var probe = (IfStatementSyntax)block.Statements.Last();
            var equal = (BinaryExpressionSyntax)TreeHelper.GetInnerExpression(probe.Condition);
            var mismatch = BinaryExpression(SyntaxKind.NotEqualsExpression, equal.Left, equal.Right);
            return block.WithStatements(block.Statements.RemoveAt(block.Statements.Count - 1)
                .Add(IfStatement(mismatch, ContinueStatement())).Add(probe.Statement));
        });

        var iterator = (LocalDeclarationStatementSyntax)scope.Statements.Single(s => LocalName(s) == "currMatch");
        if (loop.Declaration != null || loop.Initializers.Count != 0)
            throw new InvalidOperationException("Unexpected candidate iterator initialization.");
        var inside = new List<StatementSyntax>();
        foreach (var statement in scope.Statements)
        {
            if (statement == iterator)
                continue;
            if (statement != loop)
            {
                inside.Add(statement);
                continue;
            }
            if (initialLength != null)
                inside.Add(ExpressionStatement(AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                    IdentifierName("ml"), initialLength)));
            inside.Add(loop.WithDeclaration(iterator.Declaration).WithStatement(rewritten));
        }
        return scope.WithStatements(List(inside));
    }
}

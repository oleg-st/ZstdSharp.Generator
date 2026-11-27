using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace ZstdSharp.Generator.Modify;

internal static class ImproveFseState
{
    // The public struct stores an index. Only ZSTD_decompressSequences_body needs a cached pointer.
    public static void KeepStateIndex(ProjectBuilder project)
    {
        if (!project.TryGetTypeDeclaration("ZSTD_fseState", out var type, out var file))
            throw new InvalidOperationException("Missing ZSTD_fseState.");

        var field = type.Members.OfType<FieldDeclarationSyntax>().Single(f =>
            f.Declaration.Variables.Single().Identifier.ValueText is "state" or "statePtr");
        if (field.Declaration.Variables.Single().Identifier.ValueText == "state" &&
            field.Declaration.Type.ToString() == "nuint")
            return; // zstd v1.6.0 already uses the index representation.

        if (field.Declaration.Variables.Single().Identifier.ValueText != "statePtr" ||
            !AreEquivalent(field.Declaration.Type, ParseTypeName("ZSTD_seqSymbol*")))
            throw new InvalidOperationException("Unexpected ZSTD_fseState representation.");
        file.ReplaceMember(type, type.ReplaceNode(field, ParseMemberDeclaration("public nuint state;")!));

        project.ModifyMethod("ZSTD_initFseState", (_, method) =>
        {
            var statements = method.Body!.Statements;
            var local = statements.OfType<LocalDeclarationStatementSyntax>()
                .Single(s => s.Declaration.Variables.Single().Identifier.ValueText == "state");
            var value = local.Declaration.Variables.Single().Initializer!.Value;
            var setPointer = statements.Single(s => AreEquivalent(s,
                ParseStatement("DStatePtr->statePtr = DStatePtr->table + state;")));
            var setIndex = ExpressionStatement(AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                ParseExpression("DStatePtr->state"), value));
            return method.WithBody(method.Body.WithStatements(statements.Replace(local, setIndex).RemoveAt(statements.IndexOf(setPointer))));
        });
        project.ModifyMethod("ZSTD_updateFseStateWithDInfo", (_, method) =>
        {
            var update = method.Body!.Statements.Single(s => AreEquivalent(s,
                ParseStatement("DStatePtr->statePtr = DStatePtr->table + nextState + lowBits;")));
            return method.ReplaceNode(update, ParseStatement("DStatePtr->state = nextState + lowBits;"));
        });
        project.ModifyMethod("ZSTD_decodeSequence", (_, method) =>
        {
            var loads = method.DescendantNodes().OfType<MemberAccessExpressionSyntax>()
                .Where(m => m.Name.Identifier.ValueText == "statePtr");
            return method.ReplaceNodes(loads, (load, _) => ParseExpression($"{load.Expression}.table + {load.Expression}.state"));
        });
    }

    public static MethodDeclarationSyntax CacheEntryPointers(MethodDeclarationSyntax method)
    {
        var loop = method.DescendantNodes().OfType<ForStatementSyntax>().Single(f =>
            f.DescendantNodes().OfType<InvocationExpressionSyntax>().Any(IsStateUpdate));
        var parent = (BlockSyntax)loop.Parent!;
        var changed = loop;
        var locals = new List<StatementSyntax>();
        foreach (string field in new[] { "stateLL", "stateML", "stateOffb" })
        {
            var state = ParseExpression($"seqState.{field}");
            var pointer = IdentifierName(field + "Ptr");
            var lookup = ParseExpression($"{state}.table + {state}.state");
            var entry = changed.DescendantNodes().OfType<BinaryExpressionSyntax>().Single(e => AreEquivalent(e, lookup));
            changed = changed.ReplaceNode(entry, pointer);
            var call = changed.DescendantNodes().OfType<InvocationExpressionSyntax>()
                .Single(c => IsStateUpdate(c) && c.ArgumentList.Arguments.First().Expression is RefExpressionSyntax target &&
                    AreEquivalent(target.Expression, state));
            var args = call.ArgumentList.Arguments;
            if (args.Count != 5 || !args[2].RefKindKeyword.IsKind(SyntaxKind.RefKeyword) ||
                args[3].Expression is not MemberAccessExpressionSyntax { Name.Identifier.ValueText: "nextState" } ||
                args[4].Expression is not MemberAccessExpressionSyntax { Name.Identifier.ValueText: "nbBits" })
                throw new InvalidOperationException("Unexpected FSE bitstream update.");
            var read = InvocationExpression(IdentifierName("BIT_readBits"),
                ArgumentList(SeparatedList(new[] { args[1], args[2], args[4] })));
            var next = ParseExpression($"{state}.table + {args[3].Expression} + {read.NormalizeWhitespace()}");
            changed = changed.ReplaceNode<ForStatementSyntax>(call,
                AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, pointer, next));
            locals.Add(ParseStatement($"ZSTD_seqSymbol* {pointer} = {lookup};"));
        }

        var statements = parent.Statements.Replace(loop, changed).InsertRange(parent.Statements.IndexOf(loop), locals);
        return method.ReplaceNode(parent, parent.WithStatements(statements));

        static bool IsStateUpdate(InvocationExpressionSyntax call) =>
            call.Expression.ToString() == "ZSTD_updateFseStateWithDInfo";
    }
}

using System.Collections.Generic;
using System.Linq;
using ClangSharp;
using ClangSharp.Interop;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ZstdSharp.Generator.CodeGenerator;

internal partial class CodeGenerator
{
    private SyntaxNode? VisitStmt(Stmt stmt)
    {
        switch (stmt)
        {
            case Expr expr:
                return VisitExpr(expr);
            case ReturnStmt returnStmt:
                return VisitReturnStmt(returnStmt);
            case DeclStmt declStmt:
                return VisitDeclStmt(declStmt);
            case IfStmt ifStmt:
                return VisitIfStmt(ifStmt);
            case CompoundStmt compoundStmt:
                return VisitCompoundStmt(compoundStmt);
            case SwitchStmt switchStmt:
                return VisitSwitchStmt(switchStmt);
            case AttributedStmt attributedStmt:
                return VisitAttributedStmt(attributedStmt);
            case SwitchCase switchCase:
                return VisitSwitchCase(switchCase);
            case BreakStmt breakStmt:
                return VisitBreakStmt(breakStmt);
            case ForStmt forStmt:
                return VisitForStmt(forStmt);
            case WhileStmt whileStmt:
                return VisitWhileStmt(whileStmt);
            case ContinueStmt continueStmt:
                return VisitContinueStmt(continueStmt);
            case NullStmt nullStmt:
                return VisitNullStmt(nullStmt);
            case GotoStmt gotoStmt:
                return VisitGotoStmt(gotoStmt);
            case LabelStmt labelStmt:
                return VisitLabelStmt(labelStmt);
            case DoStmt doStmt:
                return VisitDoStmt(doStmt);
            default:
                Report(DiagnosticLevel.Error, $"Type {stmt.GetType()} is not implemented yet");
                return null;
        }
    }

    private SyntaxNode? VisitDoStmt(DoStmt doStmt)
    {
        // do { ... } while (false) => ...
        var cond = Visit<ExpressionSyntax>(doStmt.Cond)!;
        var constantCond = GetConstantCond(cond);
        if (constantCond.HasValue && !constantCond.Value)
        {
            return Visit<StatementSyntax>(doStmt.Body);
        }

        return SyntaxFactory.DoStatement(
            VisitStatementSyntax(doStmt.Body)!,
            cond);
    }

    private SyntaxNode VisitLabelStmt(LabelStmt labelStmt)
    {
        return SyntaxFactory.LabeledStatement(SyntaxFactory.Identifier(EscapeName(labelStmt.Decl.Name)), Visit<StatementSyntax>(labelStmt.SubStmt)!);
    }

    private SyntaxNode VisitGotoStmt(GotoStmt gotoStmt)
    {
        return SyntaxFactory.GotoStatement(SyntaxKind.GotoStatement, SyntaxFactory.IdentifierName(EscapeName(gotoStmt.Label.Name)));
    }

    private SyntaxNode VisitNullStmt(NullStmt nullStmt)
    {
        return SyntaxFactory.EmptyStatement();
    }

    private SyntaxNode? VisitContinueStmt(ContinueStmt continueStmt)
    {
        return SyntaxFactory.ContinueStatement();
    }

    private SyntaxNode? VisitWhileStmt(WhileStmt whileStmt)
    {
        var cond = Visit<ExpressionSyntax>(whileStmt.Cond)!;
        var constantCond = GetConstantCond(cond);
        // while (false)
        if (constantCond.HasValue && !constantCond.Value)
        {
            return null;
        }

        return SyntaxFactory.WhileStatement(cond, VisitStatementSyntax(whileStmt.Body)!);
    }

    private SeparatedSyntaxList<ExpressionSyntax>? GetExpressions(Stmt? stmt, out VariableDeclarationSyntax? variableDeclarationSyntax)
    {
        variableDeclarationSyntax = default;

        if (stmt == null)
        {
            return null;
        }

        var expressions = new List<ExpressionSyntax>();
        RegisterStatementConsumer(syntax =>
        {
            if (syntax is ExpressionStatementSyntax expressionStatementSyntax)
            {
                expressions.Add(expressionStatementSyntax.Expression);
            }
        });

        var node = Visit(stmt);
        switch (node)
        {
            case ExpressionSyntax expressionSyntax:
                expressions.Add(expressionSyntax);
                break;
            case LocalDeclarationStatementSyntax localDeclarationStatement:
                variableDeclarationSyntax = localDeclarationStatement.Declaration;
                break;
        }

        UnRegisterStatementConsumer();

        return expressions.Count > 0 ? SyntaxFactory.SeparatedList(expressions) : null;
    }

    private SyntaxNode VisitForStmt(ForStmt forStmt)
    {
        SeparatedSyntaxList<ExpressionSyntax> initializers = default;
        VariableDeclarationSyntax? variableDeclarationSyntax = default;

        if (forStmt.ConditionVariableDeclStmt != null)
        {
            Report(DiagnosticLevel.Error, "forStmt.ConditionVariableDeclStmt is not implemented");
        }
        else if (forStmt.Init != null)
        {
            initializers = GetExpressions(forStmt.Init, out variableDeclarationSyntax) ?? default;
        }

        return SyntaxFactory.ForStatement(variableDeclarationSyntax, initializers, forStmt.Cond != null ? Visit<ExpressionSyntax>(forStmt.Cond)! : null, GetExpressions(forStmt.Inc, out _) ?? default, VisitStatementSyntax(forStmt.Body)!);
    }

    private SyntaxNode VisitBreakStmt(BreakStmt breakStmt)
    {
        return SyntaxFactory.BreakStatement();
    }

    private SyntaxNode? VisitAttributedStmt(AttributedStmt attributedStmt)
    {
        return Visit(attributedStmt.SubStmt);
    }

    private SyntaxNode VisitSwitchCase(SwitchCase switchCase)
    {
        var switchLabelList = new SyntaxList<SwitchLabelSyntax>();
        do
        {
            if (switchCase is CaseStmt caseStmt)
            {
                switchLabelList =
                    switchLabelList.Add(SyntaxFactory.CaseSwitchLabel(Visit<ExpressionSyntax>(caseStmt.LHS)!));
            }
            else
            {
                switchLabelList =
                    switchLabelList.Add(SyntaxFactory.DefaultSwitchLabel());
            }

            var body = switchCase.SubStmt;

            if (body is SwitchCase innerSwitchCase)
            {
                switchCase = innerSwitchCase;
            }
            else
            {
                var statementList = new SyntaxList<StatementSyntax>();
                if (body != null)
                {
                    var statementSyntax = Visit<StatementSyntax>(body);
                    if (!TreeHelper.IsEmptyStatement(statementSyntax))
                    {
                        statementList = statementList.Add(statementSyntax);
                    }
                }

                return SyntaxFactory.SwitchSection(switchLabelList, statementList);
            }
        } while (true);
    }

    private SyntaxNode VisitSwitchStmt(SwitchStmt switchStmt)
    {
        var switchSectionList = new SyntaxList<SwitchSectionSyntax>();

        if (switchStmt.Body is CompoundStmt compoundStmt)
        {
            SwitchSectionSyntax? currentSectionSyntax = null;

            var fallThrough = false;
            foreach (var stmt in compoundStmt.Body)
            {
                var syntaxNode = Visit(stmt);

                if (currentSectionSyntax != null && stmt is AttributedStmt attributedStmt &&
                    attributedStmt.Attrs.Any(attr => attr.Kind == CX_AttrKind.CX_AttrKind_FallThrough))
                {
                    fallThrough = true;
                }

                if (syntaxNode is EmptyStatementSyntax || (syntaxNode is StatementSyntax statementSyntax && TreeHelper.IsEmptyStatement(statementSyntax)))
                {
                    continue;
                }

                if (syntaxNode is SwitchSectionSyntax switchSectionSyntax)
                {
                    if (currentSectionSyntax != null)
                    {
                        if (fallThrough && currentSectionSyntax.Statements.Any() && currentSectionSyntax.Labels.Any())
                        {
                            var firstLabel = switchSectionSyntax.Labels.First();

                            if (firstLabel is DefaultSwitchLabelSyntax)
                            {
                                currentSectionSyntax = currentSectionSyntax.AddStatements(
                                    SyntaxFactory.GotoStatement(SyntaxKind.GotoCaseStatement,
                                        SyntaxFactory.Token(SyntaxKind.DefaultKeyword), null!));
                            }
                            else if (firstLabel is CaseSwitchLabelSyntax caseSwitchLabelSyntax)
                            {
                                currentSectionSyntax = currentSectionSyntax.AddStatements(
                                    SyntaxFactory.GotoStatement(SyntaxKind.GotoCaseStatement,
                                        SyntaxFactory.Token(SyntaxKind.CaseKeyword),
                                        caseSwitchLabelSyntax.Value));
                            }
                        }

                        fallThrough = false;

                        switchSectionList = switchSectionList.Add(currentSectionSyntax);
                    }

                    currentSectionSyntax = switchSectionSyntax;
                }
                else if (currentSectionSyntax != null)
                {
                    if (syntaxNode != null)
                    {
                        currentSectionSyntax =
                            currentSectionSyntax.AddStatements(CastNode<StatementSyntax>(syntaxNode));
                    }
                }
                else
                {
                    Report(DiagnosticLevel.Error, "Switch parse error");
                }
            }

            if (currentSectionSyntax != null)
            {
                if (currentSectionSyntax.Labels.LastOrDefault() is DefaultSwitchLabelSyntax)
                {
                    var statementSyntax = GetStatements(currentSectionSyntax.Statements)
                        .LastOrDefault(statement => !TreeHelper.IsEmptyStatement(statement));

                    if (statementSyntax is not BreakStatementSyntax && statementSyntax is not ReturnStatementSyntax)
                    {
                        currentSectionSyntax = currentSectionSyntax.AddStatements(SyntaxFactory.BreakStatement());
                    }
                }

                switchSectionList = switchSectionList.Add(currentSectionSyntax);
            }
        }
        else
        {
            Report(DiagnosticLevel.Error, $"Unknown switchStmt Body {switchStmt.Body.GetType()}");
        }

        return SyntaxFactory.SwitchStatement(Visit<ExpressionSyntax>(switchStmt.Cond)!, switchSectionList);
    }

    private SyntaxNode VisitCompoundStmt(CompoundStmt compoundStmt)
    {
        var statements = new List<StatementSyntax>();

        RegisterStatementConsumer(statementSyntax =>
        {
            if (!TreeHelper.IsEmptyStatement(statementSyntax))
            {
                statements.Add(statementSyntax);
            }
        });

        foreach (var statement in compoundStmt.Body)
        {
            var statementSyntax = Visit<StatementSyntax>(statement);
            if (!TreeHelper.IsEmptyStatement(statementSyntax))
            {
                statements.Add(statementSyntax);
            }
        }

        UnRegisterStatementConsumer();

        // collapse block
        if (statements.Count == 1 && statements.First() is BlockSyntax)
        {
            return statements.First();
        }

        return SyntaxFactory.Block(statements);
    }

    private IEnumerable<StatementSyntax?> GetStatements(IEnumerable<StatementSyntax?> statements)
    {
        foreach (var statement in statements)
        {
            if (statement is BlockSyntax blockSyntax)
            {
                foreach (var innerStatement in GetStatements(blockSyntax.Statements))
                {
                    yield return innerStatement;
                }
            }
            else
            {
                yield return statement;
            }
        }
    }

    private SyntaxNode? VisitIfStmt(IfStmt ifStmt)
    {
        var ifCond = Visit<ExpressionSyntax>(ifStmt.Cond);
        if (ifCond == null)
        {
            return null;
        }

        var constantCond = GetConstantCond(ifCond);
        // if (true) -> then / if (false) -> else
        if (constantCond.HasValue)
        {
            var stmt = constantCond.Value
                ? VisitStatementSyntax(ifStmt.Then)
                : ifStmt.Else != null ? VisitStatementSyntax(ifStmt.Else) : null;

            return !TreeHelper.IsEmptyStatement(stmt) ? stmt : null;
        }

        // pure cond && empty then && empty else
        var thenStatement = VisitStatementSyntax(ifStmt.Then);
        var elseStatement = ifStmt.Else != null ? VisitStatementSyntax(ifStmt.Else) : null;
        if (TreeHelper.IsEmptyStatement(thenStatement) && TreeHelper.IsEmptyStatement(elseStatement) && IsPureExpr(ifCond))
        {
            return null;
        }

        // if (X) {} else {...} -> if (!(X)) {...}
        if (TreeHelper.IsEmptyStatement(thenStatement) && !TreeHelper.IsEmptyStatement(elseStatement))
        {
            // negate condition
            return SyntaxFactory.IfStatement(NegateLogicalExpression(ifCond), elseStatement);
        }

        return SyntaxFactory.IfStatement(ifCond,
            thenStatement ?? SyntaxFactory.EmptyStatement(),
            !TreeHelper.IsEmptyStatement(elseStatement) ? SyntaxFactory.ElseClause(elseStatement) : null);
    }

    private StatementSyntax? VisitStatementSyntax(Cursor cursor)
    {
        var list = new List<StatementSyntax>();

        RegisterStatementConsumer(syntax =>
        {
            list.Add(syntax);
        });

        var node = Visit<StatementSyntax>(cursor);
        UnRegisterStatementConsumer();

        if (list.Count > 0)
        {
            if (node is BlockSyntax blockSyntax)
            {
                return blockSyntax
                    .WithStatements(SyntaxFactory.List(list.Concat(blockSyntax.Statements)));
            }

            if (node != null)
            {
                list.Add(node);
            }

            return SyntaxFactory.Block()
                .WithStatements(SyntaxFactory.List(list));
        }

        return node;
    }

    private SyntaxNode? VisitDeclStmt(DeclStmt declStmt)
    {
        if (declStmt.IsSingleDecl)
        {
            return Visit(declStmt.SingleDecl!);
        }

        LocalDeclarationStatementSyntax? result = null;
        foreach (var decl in declStmt.Decls)
        {
            var localDeclarationStatementSyntax = Visit<LocalDeclarationStatementSyntax>(decl);
            if (localDeclarationStatementSyntax != null)
            {
                result = result == null
                    ? localDeclarationStatementSyntax
                    : result.AddDeclarationVariables(localDeclarationStatementSyntax.Declaration.Variables.ToArray());
            }
        }

        return result;
    }

    private SyntaxNode VisitReturnStmt(ReturnStmt returnStmt)
    {
        return SyntaxFactory.ReturnStatement(returnStmt.RetValue != null
            ? Visit<ExpressionSyntax>(returnStmt.RetValue)
            : null);
    }
}
using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using ClangSharp;
using ClangSharp.Interop;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator.Reporter;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

namespace ZstdSharp.Generator.CodeGenerator;

internal partial class CodeGenerator
{
    public ProjectBuilderConfig Config { get; }
    private readonly LinkedList<Cursor> _context;
    private readonly Stack<Action<StatementSyntax>> _statementConsumers = new();
    private readonly ProjectBuilder _projectBuilder;
    private readonly Stack<FileBuilder> _builders = new();
    private readonly Stack<Action<MemberDeclarationSyntax>> _memberConsumers = new ();
    private readonly TypeCaster.TypeCaster _typeCaster;
    private readonly CallReplacer _callReplacer;
    private readonly IReporter _reporter;
    private readonly HashSet<CXSourceRange> _visitedComments = new();

    public CodeGenerator(ProjectBuilder projectBuilder)
    {
        _projectBuilder = projectBuilder;
        Config = projectBuilder.Config;
        _context = new LinkedList<Cursor>();
        _reporter = projectBuilder.Reporter;

        _callReplacer = new CallReplacer(Config.CallReplacements);
        _typeCaster = new TypeCaster.TypeCaster(this, _callReplacer);
    }

    private string[] GetComments(Cursor cursor)
    {
        var comment = cursor.Handle.RawCommentText.CString;
        if (string.IsNullOrEmpty(comment) || !_visitedComments.Add(cursor.Handle.CommentRange))
            return [];

        return comment.Split('\n');
    }

    private T WithComments<T>(T node, Cursor cursor) where T : SyntaxNode
    {
        var comments = GetComments(cursor);
        return comments.Length > 0 ? node.WithLeadingTrivia(comments.Select(s =>
        {
            var trim = s.Trim();
            /*
             * Indent like this
             */
            if (trim.StartsWith("*"))
            {
                trim = " " + trim;
            }
            return SyntaxFactory.Comment(trim);
        })) : node;
    }

    public void Generate(TranslationUnit translationUnit)
    {
        Visit(translationUnit.TranslationUnitDecl);
    }

    private void Report(DiagnosticLevel level, string message)
    {
        _reporter.Report(level, message);
    }

    private SyntaxNode? UnsupportedNodeType(Cursor cursor)
    {
        Report(DiagnosticLevel.Error, $"Type {cursor.GetType()} is not implemented yet");
        return null;
    }

    private T? Visit<T>(Cursor cursor) where T : SyntaxNode
    {
        return CastNode<T>(Visit(cursor));
    }

    private SyntaxNode? Visit(Cursor cursor)
    {
        var currentContext = _context.AddLast(cursor);

        try
        {
            switch (cursor)
            {
                case Decl decl:
                    var declSyntax = VisitDecl(decl);
                    if (declSyntax != null)
                    {
                        declSyntax = WithComments(declSyntax, decl);
                    }
                    return declSyntax;
                case Stmt stmt:
                    return VisitStmt(stmt);
                default:
                    Report(DiagnosticLevel.Error, $"Type {cursor.GetType()} is not implemented yet");
                    return null;
            }
        }
        finally
        {
            if (_context.Last != currentContext)
            {
                Report(DiagnosticLevel.Error, "Context is corrupted");
            }
            _context.RemoveLast();
        }
    }

    private static StatementSyntax ExpressionToStatement(ExpressionSyntax expressionSyntax)
    {
        while (true)
        {
            // (E) => E
            if (expressionSyntax is ParenthesizedExpressionSyntax parenthesizedExpression)
            {
                expressionSyntax = parenthesizedExpression.Expression;
                continue;
            }

            // C ? X : Y => if (C) X else Y
            if (expressionSyntax is ConditionalExpressionSyntax conditionalExpressionSyntax)
            {
                return SyntaxFactory.IfStatement(conditionalExpressionSyntax.Condition, SyntaxFactory
                    .ExpressionStatement(conditionalExpressionSyntax.WhenTrue), SyntaxFactory.ElseClause(SyntaxFactory
                    .ExpressionStatement(conditionalExpressionSyntax.WhenFalse)));
            }

            // C -> C;
            return SyntaxFactory.ExpressionStatement(expressionSyntax);
        }
    }

    [return: NotNullIfNotNull("obj")]
    private static T? CastNode<T>(SyntaxNode? obj) where T : SyntaxNode
    {
        return obj switch
        {
            null => null,
            T result => result,
            ExpressionSyntax expressionSyntax when typeof(T) == typeof(StatementSyntax)
                => ExpressionToStatement(expressionSyntax) as T,
            _ => throw new Exception($"Cannot cast {obj.GetType()} to {typeof(T)}")
        };
    }

    private SyntaxKind GetAccessSpecifierKind(NamedDecl namedDecl)
    {
        SyntaxKind name;

        switch (namedDecl.Access)
        {
            case CX_CXXAccessSpecifier.CX_CXXInvalidAccessSpecifier:
            {
                if (namedDecl is FunctionDecl functionDecl)
                {
                    if (Config.UseDllExport)
                    {
                        return functionDecl.Attrs.Any(x => x.Kind == CX_AttrKind.CX_AttrKind_DLLExport)
                            ? SyntaxKind.PublicKeyword
                            : SyntaxKind.PrivateKeyword;
                    }

                    return functionDecl.IsStatic ? SyntaxKind.PrivateKeyword : SyntaxKind.PublicKeyword;
                }

                if (namedDecl is VarDecl varDecl)
                {
                    return varDecl.StorageClass == CX_StorageClass.CX_SC_Static || varDecl.IsLocalVarDecl
                        ? SyntaxKind.PrivateKeyword
                        : SyntaxKind.PublicKeyword;
                }

                // Top level declarations will have an invalid access specifier
                name = SyntaxKind.PublicKeyword;
                break;
            }

            case CX_CXXAccessSpecifier.CX_CXXPublic:
            {
                name = SyntaxKind.PublicKeyword;
                break;
            }

            case CX_CXXAccessSpecifier.CX_CXXProtected:
            {
                name = SyntaxKind.ProtectedKeyword;
                break;
            }

            case CX_CXXAccessSpecifier.CX_CXXPrivate:
            {
                name = SyntaxKind.PrivateKeyword;
                break;
            }

            default:
            {
                name = SyntaxKind.InternalKeyword;
                Report(DiagnosticLevel.Warning, $"Unknown access specifier: {namedDecl.Access}");
                break;
            }
        }

        return name;
    }

    internal TypeSyntax GetType(string type)
    {
        return SyntaxFactory.ParseTypeName(type);
    }

    private void StartFile(string name)
    {
        _builders.Push(_projectBuilder.Get(name));
    }

    private void StopFile()
    {
        _builders.Pop();
    }

    private void RegisterMemberConsumer(Action<MemberDeclarationSyntax> consumer)
    {
        _memberConsumers.Push(consumer);
    }

    private void UnRegisterMemberConsumer()
    {
        _memberConsumers.Pop();
    }

    private void AddMemberToConsumer(MemberDeclarationSyntax memberDeclarationSyntax)
    {
        _memberConsumers.Peek()(memberDeclarationSyntax);
    }

    private FileBuilder FileBuilder => _builders.Peek();

    private void AddUsing(string name)
    {
        FileBuilder.AddUsingDirective(name);
    }

    private void AddMember(MemberDeclarationSyntax memberDeclaration)
    {
        FileBuilder.AddMember(memberDeclaration);
    }

    private void AddMethodsMember(MemberDeclarationSyntax memberDeclaration)
    {
        FileBuilder.AddMethodsMember(memberDeclaration);
    }

    private void RegisterStatementConsumer(Action<StatementSyntax> consumer)
    {
        _statementConsumers.Push(consumer);
    }

    private void UnRegisterStatementConsumer()
    {
        _statementConsumers.Pop();
    }

    // add statement to top consumer
    private void AddStatementToConsumer(StatementSyntax statementSyntax)
    {
        _statementConsumers.Peek()(statementSyntax);
    }
}

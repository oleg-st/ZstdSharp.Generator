using System;
using Microsoft.CodeAnalysis;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Diagnostics.CodeAnalysis;

namespace ZstdSharp.Generator.CodeGenerator;

internal class FileBuilder
{
    public string Name { get; }
    public string NamespaceName { get; }

    public HashSet<string> UsingDirectives { get; } = new();

    public List<MemberDeclarationSyntax> Members { get; } = new();
    public List<MemberDeclarationSyntax> MethodsMembers { get; } = new();

    private readonly Dictionary<string, MethodDeclarationSyntax> _methodDeclarations = new();

    private const string MethodsClassName = "Methods";

    public event EventHandler<string>? MethodAdded;

    public FileBuilder(string name, string namespaceName)
    {
        Name = name;
        NamespaceName = namespaceName;
    }

    public void AddUsingDirective(params string[] names)
    {
        lock (this)
        {
            foreach (var name in names)
                UsingDirectives.Add(name);
        }
    }

    public void AddMember(MemberDeclarationSyntax syntaxNode)
    {
        lock (this)
        {
            Members.Add(syntaxNode);
        }
    }

    public void AddMethodsMember(MemberDeclarationSyntax memberDeclaration)
    {
        lock (this)
        {
            MethodsMembers.Add(memberDeclaration);

            if (memberDeclaration is MethodDeclarationSyntax methodDeclarationSyntax)
            {
                var methodName = methodDeclarationSyntax.Identifier.ToString();
                _methodDeclarations.Add(methodName, methodDeclarationSyntax);
                OnMethodAdded(methodName);
            }
        }
    }

    public bool TryGetMethod(string name, [MaybeNullWhen(false)] out MethodDeclarationSyntax methodDeclarationSyntax)
        => _methodDeclarations.TryGetValue(name, out methodDeclarationSyntax);

    public bool ReplaceMethod(string name, MethodDeclarationSyntax? newMethodDeclarationSyntax)
    {
        if (!TryGetMethod(name, out var methodDeclarationSyntax))
        {
            return false;
        }

        var index = MethodsMembers.IndexOf(methodDeclarationSyntax);
        if (index < 0)
        {
            return false;
        }

        if (newMethodDeclarationSyntax != null)
        {
            _methodDeclarations[name] = newMethodDeclarationSyntax;
            MethodsMembers[index] = newMethodDeclarationSyntax;
        }
        else
        {
            _methodDeclarations.Remove(name);
            MethodsMembers.RemoveAt(index);
        }

        return true;
    }

    private UsingDirectiveSyntax GetUsingDirective(string name)
    {
        var usingDirective = SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(name));

        var ifDefined = name switch
        {
            "System.Runtime.Intrinsics.X86" => "NETCOREAPP3_0_OR_GREATER",
            "System.Runtime.Intrinsics.Arm" => "NET5_0_OR_GREATER",
            _ => null
        };

        if (ifDefined != null)
        {
            usingDirective = usingDirective
                .WithLeadingTrivia(SyntaxFactory.Trivia(
                    SyntaxFactory.IfDirectiveTrivia(
                        SyntaxFactory.IdentifierName(ifDefined),
                        true,
                        false,
                        false)))
                .WithTrailingTrivia(SyntaxFactory.Trivia(
                    SyntaxFactory.EndIfDirectiveTrivia(
                        true)));
        }

        return usingDirective;
    }

    public CompilationUnitSyntax Build()
    {
        var namespaceDeclarationSyntax = SyntaxFactory.NamespaceDeclaration(
                SyntaxFactory.IdentifierName(NamespaceName)
            ).WithMembers(new SyntaxList<MemberDeclarationSyntax>(Members));

        if (MethodsMembers.Count > 0)
        {
            // memset, memcpy, assert, GetArrayPointer, RefToPointer etc
            AddUsingDirective("static ZstdSharp.UnsafeHelper");

            namespaceDeclarationSyntax = namespaceDeclarationSyntax
                .AddMembers(SyntaxFactory
                    .ClassDeclaration(MethodsClassName)
                    .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                        SyntaxFactory.Token(SyntaxKind.UnsafeKeyword),
                        SyntaxFactory.Token(SyntaxKind.PartialKeyword)))
                    .WithMembers(new SyntaxList<MemberDeclarationSyntax>(MethodsMembers))
                );
        }

        return SyntaxFactory.CompilationUnit()
            .WithUsings(
                SyntaxFactory.List(UsingDirectives.Select(GetUsingDirective))
            )
            .WithMembers(
                SyntaxFactory.SingletonList<MemberDeclarationSyntax>(
                    namespaceDeclarationSyntax
                )
            );
    }

    protected virtual void OnMethodAdded(string methodName)
    {
        MethodAdded?.Invoke(this, methodName);
    }
}

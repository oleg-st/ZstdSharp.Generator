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

    public void AddMethodWithName(MethodDeclarationSyntax methodDeclaration, string methodName)
    {
        lock (this)
        {
            MethodsMembers.Add(methodDeclaration);
            AddMethod(methodDeclaration, methodName);
        }
    }

    public void AddMethodsMember(MemberDeclarationSyntax memberDeclaration)
    {
        lock (this)
        {
            MethodsMembers.Add(memberDeclaration);

            if (memberDeclaration is MethodDeclarationSyntax methodDeclaration)
            {
                AddMethod(methodDeclaration, methodDeclaration.Identifier.ToString());
            }
        }
    }

    private void AddMethod(MethodDeclarationSyntax methodDeclaration, string methodName)
    {
        _methodDeclarations.Add(methodName, methodDeclaration);
        OnMethodAdded(methodName);
    }

    public bool TryGetMethod(string name, [MaybeNullWhen(false)] out MethodDeclarationSyntax methodDeclarationSyntax)
        => _methodDeclarations.TryGetValue(name, out methodDeclarationSyntax);

    public bool ReplaceMember(MemberDeclarationSyntax oldDeclarationSyntax, MemberDeclarationSyntax newMemberDeclarationSyntax)
    {
        var index = Members.IndexOf(oldDeclarationSyntax);
        if (index < 0)
            return false;
        
        Members[index] = newMemberDeclarationSyntax;
        return true;
    }

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

    private string? GetConditionForUsing(string name)
        => name switch
        {
            "System.Runtime.Intrinsics" or "System.Runtime.Intrinsics.X86" => "NETCOREAPP3_0_OR_GREATER",
            "System.Runtime.Intrinsics.Arm" => "NET5_0_OR_GREATER",
            _ => null
        };

    private IEnumerable<UsingDirectiveSyntax> GetUsingDirectives()
    {
        foreach (var kvp in UsingDirectives.GroupBy(GetConditionForUsing, u => u))
        {
            var ifDefined = kvp.Key;
            var usingDirectives = kvp.Select(name => SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(name))).ToList();
            if (ifDefined != null)
            {
                usingDirectives[0] = usingDirectives[0]
                    .WithLeadingTrivia(SyntaxFactory.Trivia(
                        SyntaxFactory.IfDirectiveTrivia(
                            SyntaxFactory.IdentifierName(ifDefined),
                            true,
                            false,
                            false)));
                usingDirectives[^1] = usingDirectives[^1]
                    .WithTrailingTrivia(SyntaxFactory.Trivia(
                        SyntaxFactory.EndIfDirectiveTrivia(
                            true)));
            }

            foreach (var usingDirective in usingDirectives)
            {
                yield return usingDirective;
            }
        }
    }

    public CompilationUnitSyntax Build()
    {
        var namespaceDeclarationSyntax = SyntaxFactory.NamespaceDeclaration(
                SyntaxFactory.IdentifierName(NamespaceName)
            ).WithMembers(new SyntaxList<MemberDeclarationSyntax>(Members));

        if (MethodsMembers.Count > 0)
        {
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
                SyntaxFactory.List(GetUsingDirectives())
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

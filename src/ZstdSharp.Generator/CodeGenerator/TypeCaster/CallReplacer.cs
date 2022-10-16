using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using ClangSharp;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ZstdSharp.Generator.CodeGenerator.TypeCaster;

internal class CallReplacer
{
    private readonly Dictionary<string, CallReplacement> _callReplacements;

    public abstract class CallReplacement
    {
        public TypeCaster.CustomType Type { get; }

        public string? UsingDirective { get; }

        public TypeCaster.CustomType[]? ArgumentTypes { get; }

        public bool WithArguments => ArgumentTypes != null;

        protected CallReplacement(TypeCaster.CustomType type, string? usingDirective = null, TypeCaster.CustomType[]? argumentTypes = null)
        {
            Type = type;
            UsingDirective = usingDirective;
            ArgumentTypes = argumentTypes;
        }

        public TypeCaster.CustomType? GetArgumentType(int index)
        {
            return ArgumentTypes != null && index < ArgumentTypes.Length ? ArgumentTypes[index] : null;
        }

        public abstract SyntaxNode? Apply(InvocationExpressionSyntax invocationExpressionSyntax);
    }

    public class CallReplacementInvocation : CallReplacement
    {
        public string Replacement { get; }

        public override SyntaxNode Apply(InvocationExpressionSyntax invocationExpressionSyntax)
        {
            return SyntaxFactory.InvocationExpression(SyntaxFactory.IdentifierName(Replacement))
                .WithArgumentList(invocationExpressionSyntax.ArgumentList);
        }

        public CallReplacementInvocation(string replacement, TypeCaster.CustomType type, string? usingDirective = null, TypeCaster.CustomType[]? argumentTypes = null) : base(type, usingDirective, argumentTypes)
        {
            Replacement = replacement;
        }
    }

    public class CallReplacementExpression: CallReplacement
    {
        public ExpressionSyntax ExpressionSyntax{ get; }

        public override SyntaxNode Apply(InvocationExpressionSyntax invocationExpressionSyntax)
        {
            return ExpressionSyntax;
        }

        public CallReplacementExpression(string expression, TypeCaster.CustomType type, string? usingDirective = null) : base(type, usingDirective)
        {
            ExpressionSyntax = SyntaxFactory.ParseExpression(expression);
        }
    }

    public class CallReplacementIdentity: CallReplacement
    {
        public override SyntaxNode Apply(InvocationExpressionSyntax invocationExpressionSyntax)
        {
            return invocationExpressionSyntax;
        }

        public CallReplacementIdentity(TypeCaster.CustomType type, string? usingDirective = null) : base(type, usingDirective)
        {
        }
    }

    public class CallReplacementRemove: CallReplacement
    {
        public override SyntaxNode? Apply(InvocationExpressionSyntax invocationExpressionSyntax)
        {
            return null;
        }

        public CallReplacementRemove() : base(new TypeCaster.OtherType())
        {
        }
    }

    public CallReplacer(IReadOnlyDictionary<string, CallReplacement> callReplacements)
    {
        _callReplacements = new Dictionary<string, CallReplacement>(callReplacements);
    }

    public CallReplacement? GetCallReplacement(CallExpr callExpr)
    {
        return !string.IsNullOrEmpty(callExpr.Callee.Spelling) &&
               TryGetValue(callExpr.Callee.Spelling, out var callReplacement)
            ? callReplacement
            : null;
    }

    public CallReplacement? GetCallReplacement(string name)
    {
        return _callReplacements.TryGetValue(name, out var callReplacement)
            ? callReplacement
            : null;
    }

    public bool TryGetValue(string name, [MaybeNullWhen(false)] out CallReplacement callReplacement) 
        => _callReplacements.TryGetValue(name, out callReplacement);
}

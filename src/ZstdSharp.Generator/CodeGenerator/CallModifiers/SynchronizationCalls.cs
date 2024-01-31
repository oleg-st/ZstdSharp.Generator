using System;
using Microsoft.CodeAnalysis;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

namespace ZstdSharp.Generator.CodeGenerator.CallModifiers;

internal class SynchronizationCalls : ICallsModifier
{
    private class CallReplacementPulse: CallReplacer.CallReplacement
    {
        public string Replacement { get; }

        public override SyntaxNode Apply(InvocationExpressionSyntax invocationExpressionSyntax)
        {
            var arg = invocationExpressionSyntax.ArgumentList.Arguments[0];
            var expr = TreeHelper.GetInnerExpression(arg.Expression).ToString();

            // cond -> mutex, Cond -> Mutex, beware of reusing mutex for multiple condition variables
            if (expr.Contains("cond"))
            {
                expr = expr.Replace("cond", "mutex");
            } else if (expr.Contains("Cond"))
            {
                expr = expr.Replace("Cond", "Mutex");
            }
            else
            {
                throw new Exception($"Failed to determine an object for condition variable {expr}");
            }

            var expression = SyntaxFactory.ParseExpression(expr);
            return SyntaxFactory.InvocationExpression(SyntaxFactory.IdentifierName(Replacement))
                .WithArgumentList(SyntaxFactory.ArgumentList(SyntaxFactory.SingletonSeparatedList(SyntaxFactory.Argument(expression))));
        }

        public CallReplacementPulse(string replacement, TypeCaster.TypeCaster.CustomType type, string? usingDirective = null, TypeCaster.TypeCaster.CustomType[]? argumentTypes = null) : base(type, usingDirective, argumentTypes)
        {
            Replacement = replacement;
        }
    }

    public IEnumerable<(string, CallReplacer.CallReplacement)> GetCallReplacements()
    {
        yield return ("synchronization_init",
            new CallReplacer.CallReplacementInvocation("SynchronizationWrapper.Init", new TypeCaster.TypeCaster.VoidType()));
        yield return ("synchronization_free",
            new CallReplacer.CallReplacementInvocation("SynchronizationWrapper.Free", new TypeCaster.TypeCaster.VoidType()));
        yield return ("synchronization_enter",
            new CallReplacer.CallReplacementInvocation("SynchronizationWrapper.Enter", new TypeCaster.TypeCaster.VoidType()));
        yield return ("synchronization_exit",
            new CallReplacer.CallReplacementInvocation("SynchronizationWrapper.Exit", new TypeCaster.TypeCaster.VoidType()));
        yield return ("synchronization_wait",
            new CallReplacer.CallReplacementInvocation("SynchronizationWrapper.Wait", new TypeCaster.TypeCaster.VoidType()));
        yield return ("synchronization_pulse",
            new CallReplacementPulse("SynchronizationWrapper.Pulse", new TypeCaster.TypeCaster.VoidType()));
        yield return ("synchronization_pulse_all",
            new CallReplacementPulse("SynchronizationWrapper.PulseAll", new TypeCaster.TypeCaster.VoidType()));
    }

    public IEnumerable<(string, string)> GetMacros()
    {
        yield return ("ZSTD_pthread_mutex_t", "ZSTD_pthread_mutex_t void*");
        yield return ("ZSTD_pthread_mutex_init",
            "ZSTD_pthread_mutex_init(a, b) ((void)(b), synchronization_init((a)), 0)");
        yield return ("ZSTD_pthread_mutex_destroy", "ZSTD_pthread_mutex_destroy(a) synchronization_free((a))");
        yield return ("ZSTD_pthread_mutex_lock", "ZSTD_pthread_mutex_lock(a) synchronization_enter((a))");
        yield return ("ZSTD_pthread_mutex_unlock", "ZSTD_pthread_mutex_unlock(a) synchronization_exit((a))");

        yield return ("ZSTD_pthread_cond_t", "ZSTD_pthread_cond_t void*");
        yield return ("ZSTD_pthread_cond_init", "ZSTD_pthread_cond_init(a, b) ((void)(a), (void)(b), 0)");
        yield return ("ZSTD_pthread_cond_destroy", "ZSTD_pthread_cond_destroy(a) ((void)(a))");
        yield return ("ZSTD_pthread_cond_wait", "ZSTD_pthread_cond_wait(a, b) synchronization_wait((b))");
        yield return ("ZSTD_pthread_cond_signal", "ZSTD_pthread_cond_signal(a) synchronization_pulse((a))");
        yield return ("ZSTD_pthread_cond_broadcast", "ZSTD_pthread_cond_broadcast(a) synchronization_pulse_all((a))");
    }

    public string GetDefinitions() =>
        "void synchronization_init(void **obj);\n" +
        "void synchronization_free(void **obj);\n" +
        "void synchronization_enter(void **obj);\n" +
        "void synchronization_exit(void **obj);\n" +
        "void synchronization_wait(void **obj);\n" +
        "void synchronization_pulse(void **obj);\n" +
        "void synchronization_pulse_all(void **obj);\n";
}

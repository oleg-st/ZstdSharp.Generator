using System;

namespace ZstdSharp.Generator.CodeGenerator.Reporter;

internal class Reporter : IReporter
{
    public void Report(DiagnosticLevel level, string message)
    {
        // todo
        Console.WriteLine($"========== Report {level} {message}");
    }
}

using System;

namespace ZstdSharp.Generator.CodeGenerator.Reporter;

internal class Reporter : IReporter
{
    public void Report(DiagnosticLevel level, string message)
    {
        // todo
        Console.ForegroundColor = level switch
        {
            DiagnosticLevel.Error => ConsoleColor.Red,
            DiagnosticLevel.Warning => ConsoleColor.Yellow,
            DiagnosticLevel.Info => ConsoleColor.Green,
            _ => ConsoleColor.White,
        };
        Console.WriteLine($"========== Report {level} {message}");
        Console.ResetColor();
    }
}

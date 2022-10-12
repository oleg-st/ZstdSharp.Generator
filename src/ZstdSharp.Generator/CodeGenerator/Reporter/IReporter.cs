namespace ZstdSharp.Generator.CodeGenerator.Reporter;

internal interface IReporter
{
    void Report(DiagnosticLevel level, string message);
}

using Microsoft.CodeAnalysis;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator.Reporter;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

namespace ZstdSharp.Generator.CodeGenerator.CallModifiers;

internal class DisplayCallsGetter : ICallsModifier
{
    private readonly IReporter _reporter;

    private static class ParseFormatter
    {
        // https://stackoverflow.com/questions/6915025/regexp-to-detect-if-a-string-has-printf-placeholders-inside
        private static readonly Regex PlaceHolder = new(
            "%(?:(?<index>[1-9]\\d*)\\$|\\((?<name>[^\\)]+)\\))?(\\+)?(0|'[^$])?(-)?(?<alignment>\\d+)?(?:\\.(?<precision>\\d+))?(?<type>[b-fiosuxX])");

        public record Placeholder(int? Index, string? Name, int? Alignment, int? Precision, string Type, Match Match);

        private static Placeholder FromMatch(Match match, ref int autoIndex)
        {
            var indexValue = match.Groups["index"].Value;
            var name = match.Groups["name"].Value;
            var alignmentValue = match.Groups["alignment"].Value;
            var precisionValue = match.Groups["precision"].Value;
            var type = match.Groups["type"].Value;

            int? index;
            if (!string.IsNullOrEmpty(name))
            {
                index = null;
            }
            else
            {
                name = null;
                index = !string.IsNullOrEmpty(indexValue) ? int.Parse(indexValue) : autoIndex++;
            }


            int? alignment = !string.IsNullOrEmpty(alignmentValue) ? int.Parse(alignmentValue) : null;
            int? precision = !string.IsNullOrEmpty(precisionValue) ? int.Parse(precisionValue) : null;
            return new Placeholder(index, name, alignment, precision, type, match);
        }

        public static IEnumerable<Placeholder> Parse(string format)
        {
            var matches = PlaceHolder.Matches(format);
            var autoIndex = 1;
            foreach (Match match in matches)
            {
                yield return FromMatch(match, ref autoIndex);
            }
        }
    }

    private class CallReplacementDisplay : CallReplacer.CallReplacement
    {
        private readonly IReporter _reporter;

        public override SyntaxNode Apply(InvocationExpressionSyntax invocationExpressionSyntax)
        {
            var argumentListSyntax = invocationExpressionSyntax.ArgumentList;
            if (argumentListSyntax.Arguments.Count >= 2)
            {
                var argumentSyntax = argumentListSyntax.Arguments[1];
                if (argumentSyntax.Expression is LiteralExpressionSyntax literalExpression &&
                    literalExpression.Kind() == SyntaxKind.StringLiteralExpression &&
                    literalExpression.Token.Value is string formatValue)
                {
                    var interpolatedStringExpression = SyntaxFactory
                        .InterpolatedStringExpression(SyntaxFactory.Token(SyntaxKind.InterpolatedStringStartToken))
                        .WithContents(SyntaxFactory.List(GetParts(formatValue)));

                    return invocationExpressionSyntax
                        .WithArgumentList(SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(
                            new[]
                            {
                                argumentListSyntax.Arguments[0],
                                SyntaxFactory.Argument(interpolatedStringExpression)
                            })));

                    IEnumerable<InterpolatedStringContentSyntax> GetParts(string format)
                    {
                        int position = 0;
                        foreach (var placeholder in ParseFormatter.Parse(formatValue))
                        {
                            if (placeholder.Index == null)
                            {
                                _reporter.Report(DiagnosticLevel.Error, "Named argument is not supported");
                            }
                            else
                            {
                                var argument = argumentListSyntax.Arguments[1 + placeholder.Index.Value];
                                var matchIndex = placeholder.Match.Index;
                                var matchLength = placeholder.Match.Length;

                                if (placeholder.Match.Index > position)
                                {
                                    var text = format.Substring(position, placeholder.Match.Index - position);
                                    yield return SyntaxFactory.InterpolatedStringText()
                                        .WithTextToken(SyntaxFactory.Token(SyntaxFactory.TriviaList(),
                                            SyntaxKind.InterpolatedStringTextToken,
                                            SymbolDisplay.FormatLiteral(text, false), text,
                                            SyntaxFactory.TriviaList()));
                                }

                                var interpolation = SyntaxFactory.Interpolation(argument.Expression);
                                if (placeholder.Alignment != null)
                                {
                                    interpolation = interpolation.WithAlignmentClause(
                                        SyntaxFactory.InterpolationAlignmentClause(
                                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                                            SyntaxFactory.LiteralExpression(
                                                SyntaxKind.NumericLiteralExpression,
                                                SyntaxFactory.Literal(placeholder.Alignment.Value))));
                                }

                                if (placeholder.Precision != null)
                                {
                                    var formatString = $"0.{new string('0', placeholder.Precision.Value)}";
                                    interpolation = interpolation.WithFormatClause(
                                        SyntaxFactory.InterpolationFormatClause(
                                                SyntaxFactory.Token(SyntaxKind.ColonToken))
                                            .WithFormatStringToken(
                                                SyntaxFactory.Token(
                                                    SyntaxFactory.TriviaList(),
                                                    SyntaxKind.InterpolatedStringTextToken,
                                                    formatString,
                                                    formatString,
                                                    SyntaxFactory.TriviaList())));
                                }

                                yield return interpolation;
                                position = matchIndex + matchLength;
                            }
                        }

                        if (position < format.Length)
                        {
                            var text = format.Substring(position);
                            yield return SyntaxFactory.InterpolatedStringText()
                                .WithTextToken(SyntaxFactory.Token(SyntaxFactory.TriviaList(),
                                    SyntaxKind.InterpolatedStringTextToken,
                                    SymbolDisplay.FormatLiteral(text, false), text,
                                    SyntaxFactory.TriviaList()));
                        }
                    }
                }
            }

            _reporter.Report(DiagnosticLevel.Warning, "Unrecognized Display call");
            return invocationExpressionSyntax;
        }

        public CallReplacementDisplay(IReporter reporter) : base(new TypeCaster.TypeCaster.OtherType())
        {
            _reporter = reporter;
        }
    }

    private class CallReplacementDisplayUpdate : CallReplacer.CallReplacement
    {
        private readonly IReporter _reporter;

        public override SyntaxNode Apply(InvocationExpressionSyntax invocationExpressionSyntax)
        {
            var argumentListSyntax = invocationExpressionSyntax.ArgumentList;
            if (argumentListSyntax.Arguments.Count == 3)
            {
                var argumentSyntax = argumentListSyntax.Arguments[1];
                if (argumentSyntax.Expression is LiteralExpressionSyntax literalExpression &&
                    literalExpression.Kind() == SyntaxKind.StringLiteralExpression &&
                    literalExpression.Token.Value is string formatValue)
                {
                    var placeholders = ParseFormatter.Parse(formatValue).ToList();
                    if (placeholders.Count == 1 && placeholders[0].Index == 1)
                    {
                        return invocationExpressionSyntax
                            .WithArgumentList(SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(
                                new[]
                                {
                                    argumentListSyntax.Arguments[0],
                                    argumentListSyntax.Arguments[2]
                                })));
                    }
                }
            }

            _reporter.Report(DiagnosticLevel.Warning, "Unrecognized DisplayUpdate call");
            return invocationExpressionSyntax;
        }

        public CallReplacementDisplayUpdate(IReporter reporter) : base(new TypeCaster.TypeCaster.OtherType())
        {
            _reporter = reporter;
        }
    }

    public DisplayCallsGetter(IReporter reporter)
    {
        _reporter = reporter;
    }

    public IEnumerable<(string, CallReplacer.CallReplacement)> GetCallReplacements()
    {
        yield return ("Display", new CallReplacementDisplay(_reporter));
        yield return ("DisplayUpdate", new CallReplacementDisplayUpdate(_reporter));
    }

    public IEnumerable<(string, string)> GetMacros()
    {
        yield return ("LOCALDISPLAYLEVEL", "LOCALDISPLAYLEVEL(displayLevel, l, ...) Display(l, __VA_ARGS__)");
        yield return ("LOCALDISPLAYUPDATE", "LOCALDISPLAYUPDATE(displayLevel, l, ...) DisplayUpdate(l, __VA_ARGS__)");
        yield return ("DISPLAY", "DISPLAY(...) Display(2, __VA_ARGS__)");
    }

    public string GetDefinitions() => "void Display(int level, const char *format, ...);\r\n" +
                                      "void DisplayUpdate(int level, const char *format, ...);\r\n";
}
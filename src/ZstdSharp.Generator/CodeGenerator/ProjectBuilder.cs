using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.MSBuild;
using Microsoft.CodeAnalysis.Simplification;
using ZstdSharp.Generator.CodeGenerator.Reporter;

namespace ZstdSharp.Generator.CodeGenerator;

internal class ProjectBuilder
{
    public ProjectBuilderConfig Config { get; }
    public IReporter Reporter { get; }
    private readonly Dictionary<string, FileBuilder> _builders = new();
    private readonly Dictionary<string, FileBuilder> _methodBuilders = new();
    private readonly HashSet<string> _generatedTypes = new();

    public ProjectBuilder(ProjectBuilderConfig config, IReporter reporter)
    {
        Config = config;
        Reporter = reporter;
    }

    internal bool AddGeneratedType(string name)
    {
        lock (this)
        {
            return _generatedTypes.Add(name);
        }
    }

    public FileBuilder Get(string name)
    {
        lock (this)
        {
            if (!_builders.TryGetValue(name, out var builder))
            {
                builder = _builders[name] = new FileBuilder(name, Config.NamespaceName);
                builder.MethodAdded += (sender, methodName) =>
                {
                    _methodBuilders[methodName] = (sender as FileBuilder)!;
                };
            }

            return builder;
        }
    }

    public bool TryGetBuilder(string name, [MaybeNullWhen(false)] out FileBuilder builder)
    {
        lock (this)
        {
            return _builders.TryGetValue(name, out builder);
        }
    }

    public IEnumerable<FileBuilder> GetBuilders() => _builders.Values;

    public bool TryGetTypeDeclaration(string name,
        [MaybeNullWhen(false)] out TypeDeclarationSyntax typeDeclaration)
    {
        if (!TryGetBuilder(name, out var builder))
        {
            typeDeclaration = null;
            return false;
        }

        typeDeclaration = builder.Members.OfType<TypeDeclarationSyntax>().FirstOrDefault(t => t.Identifier.ToString() == name);
        return typeDeclaration != null;
    }

    public bool TryGetMethod(string name, [MaybeNullWhen(false)] out FileBuilder fileBuilder,
        [MaybeNullWhen(false)] out MethodDeclarationSyntax methodDeclarationSyntax)
    {
        if (_methodBuilders.TryGetValue(name, out fileBuilder) &&
            fileBuilder.TryGetMethod(name, out methodDeclarationSyntax))
        {
            return true;
        }

        fileBuilder = default;
        methodDeclarationSyntax = default;
        return false;
    }

    private async Task BuildFiles()
    {
        Directory.CreateDirectory(Config.UnsafeOutputLocation);

        await Task.WhenAll(_builders.Values.Select(async builder =>
            await Task.Run(async () =>
            {
                var compilationUnitSyntax = builder.Build();

                var filename = Path.Combine(Config.UnsafeOutputLocation, $"{builder.Name}.cs");
                await File.WriteAllTextAsync(filename, compilationUnitSyntax.NormalizeWhitespace().ToFullString());
            })));
    }

    private async Task Simplify()
    {
        MSBuildLocator.RegisterDefaults();

        var workspace = MSBuildWorkspace.Create();
        workspace.WorkspaceFailed += (_, args) =>
        {
            var level = args.Diagnostic.Kind switch
            {
                WorkspaceDiagnosticKind.Failure => DiagnosticLevel.Error,
                WorkspaceDiagnosticKind.Warning => DiagnosticLevel.Warning,
                _ => DiagnosticLevel.Error
            };

            Reporter.Report(level, args.Diagnostic.Message);
        };

        var project = await workspace.OpenProjectAsync(Path.Combine(Config.OutputLocation, "ZstdSharp.csproj"));
        await Task.WhenAll(project.Documents.Select(async document =>
            await Task.Run(async () =>
            {
                // format
                document = await Formatter.FormatAsync(document);

                var root = await document.GetSyntaxRootAsync();
                if (root != null)
                {
                    var newRoot = root.WithAdditionalAnnotations(Simplifier.Annotation);
                    var newDocument = await Simplifier
                        .ReduceAsync(document.WithSyntaxRoot(newRoot));

                    var newSyntaxTree = await newDocument.GetSyntaxTreeAsync();
                    if (newSyntaxTree != null && newDocument.FilePath != null)
                    {
                        await File.WriteAllTextAsync(newDocument.FilePath, newSyntaxTree.ToString());
                    }
                }
            })));
    }

    public async Task Save()
    {
        // clean and create output directory
        if (Directory.Exists(Config.OutputLocation))
        {
            Directory.Delete(Config.OutputLocation, true);
        }

        Directory.CreateDirectory(Config.OutputLocation);

        // build files
        await BuildFiles();
        // add extra files
        await FsHelper.CopyAll(Config.SourceLocation, Config.OutputLocation);
        // simplify project
        await Simplify();
    }

    public bool HasMethod(string name) => _methodBuilders.ContainsKey(name);

    public IEnumerable<string> GetMethods() => _methodBuilders.Keys;

    public bool ModifyMethod(string name,
        Func<FileBuilder, MethodDeclarationSyntax, MethodDeclarationSyntax?> modifier)
    {
        if (!TryGetMethod(name, out var builder, out var methodDeclarationSyntax))
        {
            return false;
        }

        builder.ReplaceMethod(name, modifier(builder, methodDeclarationSyntax));
        return true;
    }
}

using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.MSBuild;
using Microsoft.CodeAnalysis.Simplification;
using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Reflection.Emit;
using System.Threading.Tasks;
using ZstdSharp.Generator.CodeGenerator.Reporter;

namespace ZstdSharp.Generator.CodeGenerator;

internal class ProjectBuilder(ProjectBuilderConfig config, IReporter reporter)
{
    public ProjectBuilderConfig Config { get; } = config;
    public IReporter Reporter { get; } = reporter;
    private readonly Dictionary<string, FileBuilder> _builders = [];
    private readonly Dictionary<string, FileBuilder> _methodBuilders = [];
    private readonly HashSet<string> _generatedTypes = [];
    private readonly HashSet<string> _generatedInitConstructor = [];
    private readonly HashSet<IReducer> _reducers = [];

    internal bool AddGeneratedType(string name)
    {
        lock (this)
        {
            return _generatedTypes.Add(name);
        }
    }

    internal bool AddInitConstructor(string name)
    {
        lock (this)
        {
            return _generatedInitConstructor.Add(name);
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
        [MaybeNullWhen(false)] out TypeDeclarationSyntax typeDeclaration, [MaybeNullWhen(false)] out FileBuilder fileBuilder)
    {
        if (!TryGetBuilder(name, out fileBuilder))
        {
            typeDeclaration = null;
            return false;
        }

        typeDeclaration = fileBuilder.Members.OfType<TypeDeclarationSyntax>().FirstOrDefault(t => t.Identifier.ToString() == name);
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

    public void RegisterReducer(IReducer reducer)
    {
        _reducers.Add(reducer);
    }

    private async Task BuildFiles()
    {
        Directory.CreateDirectory(Config.UnsafeOutputLocation);

        await Task.WhenAll(_builders.Values.Select(async builder =>
            await Task.Run(async () =>
            {
                var compilationUnitSyntax = builder.Build();
                foreach (var reducer in _reducers)
                {
                    compilationUnitSyntax = reducer.Reduce(compilationUnitSyntax);
                }

                var filename = Path.Combine(Config.UnsafeOutputLocation, $"{builder.Name}.cs");
                await File.WriteAllTextAsync(filename, compilationUnitSyntax.NormalizeWhitespace().ToFullString());
            })));
    }

    private async Task Simplify()
    {
        var preferred = MSBuildLocator.QueryVisualStudioInstances()
            .FirstOrDefault(i => i.Version.Major == Environment.Version.Major);
        if (preferred != null)
        {
            MSBuildLocator.RegisterInstance(preferred);
        }
        else
        {
            var instance = MSBuildLocator.RegisterDefaults();
            Reporter.Report(DiagnosticLevel.Warning, $"Using MSBuild {instance.Version}");
        }

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
        await FsHelper.CopyAll(Config.SourceLocation, Config.OutputLocation, Config.SourceExcludeNames);
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

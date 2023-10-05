using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using ZstdSharp.Generator.CodeGenerator.TypeCaster;

namespace ZstdSharp.Generator.CodeGenerator;

internal class ProjectBuilderConfig
{
    public string NamespaceName { get; }
    public string OutputLocation { get; }
    public string UnsafeOutputLocation { get; }
    public string SourceLocation { get; }
    public IReadOnlyDictionary<string, string> RemappedNames { get; }
    public IReadOnlyDictionary<string, CallReplacer.CallReplacement> CallReplacements { get; }
    public IReadOnlySet<string> StructToClasses { get; }
    public IReadOnlySet<string> ExcludedNames { get; }
    public IReadOnlySet<string> TraversalNames { get; }
    public IReadOnlySet<string> InlineMethods { get; }
    public IReadOnlySet<string> ExcludeFunctionPointers { get; }

    public bool UseFunctionPointers { get; }

    public bool ConvertNestedArraysToMultidimensional { get; }

    public bool ArrayCreateOptimization { get; }
    public bool UseDllExport { get; }

    public ProjectBuilderConfig(string namespaceName, string outputLocation,
        string unsafeOutputLocation, string sourceLocation,
        IReadOnlyDictionary<string, string>? remappedNames = null, IReadOnlySet<string>? excludedNames = null,
        IReadOnlySet<string>? traversalNames = null, IReadOnlySet<string>? inlineMethods = null,
        IReadOnlyDictionary<string, CallReplacer.CallReplacement>? callReplacements = null,
        IReadOnlySet<string>? structToClasses = null,
        bool useFunctionPointers = true,
        IReadOnlySet<string>? excludeFunctionPointers = null,
        bool convertNestedArraysToMultidimensional = false,
        bool arrayCreateOptimization = true,
        bool useDllExport = true)
    {
        NamespaceName = namespaceName;
        OutputLocation = outputLocation;
        UnsafeOutputLocation = unsafeOutputLocation;
        SourceLocation = sourceLocation;
        RemappedNames = remappedNames ?? ImmutableDictionary<string, string>.Empty;
        ExcludedNames = excludedNames ?? ImmutableHashSet<string>.Empty;
        TraversalNames = traversalNames?.Select(traversalName => traversalName.Replace('\\', '/')).ToHashSet() ??
                         new HashSet<string>();
        InlineMethods = inlineMethods ?? ImmutableHashSet<string>.Empty;
        CallReplacements = callReplacements ??
                           ImmutableDictionary<string, CallReplacer.CallReplacement>.Empty;
        StructToClasses = structToClasses ?? ImmutableHashSet<string>.Empty;
        UseFunctionPointers = useFunctionPointers;
        ExcludeFunctionPointers = excludeFunctionPointers ?? ImmutableHashSet<string>.Empty;
        ConvertNestedArraysToMultidimensional = convertNestedArraysToMultidimensional;
        ArrayCreateOptimization = arrayCreateOptimization;
        UseDllExport = useDllExport;
    }
}

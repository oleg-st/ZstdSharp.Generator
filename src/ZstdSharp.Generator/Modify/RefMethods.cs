using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using ZstdSharp.Generator.CodeGenerator;
using ZstdSharp.Generator.CodeGenerator.Reporter;

namespace ZstdSharp.Generator.Modify;

internal class RefMethods(ProjectBuilder projectBuilder, IReporter reporter)
{
    private readonly IReadOnlyList<RefMethodInfo> _methods =
    [
        // bit decompress
        new(projectBuilder, "BIT_initDStream", new()
        {
            {"bitD", RefMethodInfo.EmptyParameterInfo}
        }),
        new(projectBuilder, "BIT_lookBits", new()
        {
            {"bitD", new(
            [
                new("bitContainer"),
                new("bitsConsumed"),
            ])}
        }),
        new(projectBuilder, "BIT_lookBitsFast", new()
        {
            {"bitD", new(
            [
                new("bitContainer"),
                new("bitsConsumed"),
            ])}
        }),
        new(projectBuilder, "BIT_skipBits", new()
        {
            {"bitD", new(
            [
                new("bitsConsumed", true),
            ])}
        }),
        new(projectBuilder, "BIT_readBits", new()
        {
            {"bitD", new(
            [
                new("bitContainer"),
                new("bitsConsumed", true),
            ])}
        }),
        new(projectBuilder, "BIT_readBitsFast", new()
        {
            {"bitD", new(
            [
                new("bitContainer"),
                new("bitsConsumed", true),
            ])}
        }),
        new(projectBuilder, "BIT_reloadDStreamFast", new()
        {
            {"bitD", new(
            [
                new("bitContainer", true),
                new("bitsConsumed", true),
                new("ptr", true),
                new("start"),
                new("limitPtr"),
            ])}
        }),
        new(projectBuilder, "BIT_reloadDStream", new()
        {
            {"bitD", new(
            [
                new("bitContainer", true),
                new("bitsConsumed", true),
                new("ptr", true),
                new("start"),
                new("limitPtr"),
            ])}
        }),
        new(projectBuilder, "BIT_reloadDStream_internal", new()
        {
            {"bitD", new(
            [
                new("bitContainer", true),
                new("bitsConsumed", true),
                new("ptr", true),
                new("start"),
            ])}
        }),
        new(projectBuilder, "BIT_endOfDStream", new()
        {
            {"DStream", new(
            [
                new("bitsConsumed"),
                new("ptr"),
                new("start"),
            ])}
        }),
        // decompress
        new(projectBuilder, "ZSTD_initFseState", new()
        {
            {"DStatePtr", RefMethodInfo.EmptyParameterInfo}, 
            {"bitD", RefMethodInfo.EmptyParameterInfo}
        }),
        new(projectBuilder, "ZSTD_updateFseStateWithDInfo", new()
        {
            {"DStatePtr", RefMethodInfo.EmptyParameterInfo}, 
            {"bitD", new(
            [
                new("bitContainer"),
                new("bitsConsumed", true),
            ])}
        }),
        new(projectBuilder, "ZSTD_overlapCopy8", new()
        {
            {"ip", RefMethodInfo.EmptyParameterInfo}, 
            {"op", RefMethodInfo.EmptyParameterInfo}
        }),
        // bit compress
        new(projectBuilder, "BIT_initCStream", new()
        {
            {"bitC", RefMethodInfo.EmptyParameterInfo }
        }, true),
        new(projectBuilder, "BIT_addBits", new()
        {
            {"bitC", new(
            [
                new("bitContainer", true),
                new("bitPos", true),
            ])}
        }, true),
        new(projectBuilder, "BIT_addBitsFast", new()
        {
            {"bitC", new(
            [
                new("bitContainer", true),
                new("bitPos", true),
            ])}
        }, true),
        new(projectBuilder, "BIT_flushBitsFast", new()
        {
            {"bitC", new(
            [
                new("bitContainer", true),
                new("bitPos", true),
                new("ptr", true),
                new("endPtr"),
            ])}
        }, true),
        new(projectBuilder, "BIT_flushBits", new()
        {
            {"bitC", new(
            [
                new("bitContainer", true),
                new("bitPos", true),
                new("ptr", true),
                new("endPtr"),
            ])}
        }, true),
        new(projectBuilder, "BIT_closeCStream", new()
        {
            {"bitC", new(
            [
                new("bitContainer", true),
                new("bitPos", true),
                new("ptr"),
                new("endPtr"),
                new("startPtr"),
            ])}
        }, true),
        // fse compress
        new(projectBuilder, "FSE_initCState", new()
        {
            {"statePtr", RefMethodInfo.EmptyParameterInfo}
        }),
        new(projectBuilder, "FSE_initCState2", new()
        {
            {"statePtr", RefMethodInfo.EmptyParameterInfo}
        }, true),
        new(projectBuilder, "FSE_encodeSymbol", new()
        {
            {"bitC", new(
            [
                new("bitContainer", true),
                new("bitPos", true),
            ])},
            {"statePtr", RefMethodInfo.EmptyParameterInfo}
        }, true),
        new(projectBuilder, "FSE_flushCState", new()
        {
            {"bitC", new(
            [
                new("bitContainer", true),
                new("bitPos", true),
                new("ptr", true),
                new("endPtr"),
            ])},
            {"statePtr", RefMethodInfo.EmptyParameterInfo}
        }, true),
        // huf compress
        new(projectBuilder, "HUF_initCStream", new()
        {
            {"bitC", RefMethodInfo.EmptyParameterInfo}
        }, true),
        new(projectBuilder, "HUF_zeroIndex1", new()
        {
            {"bitC", new(
            [
                new("bitContainer.e1", true),
                new("bitPos.e1", true),
            ])},
        }, true),
        new(projectBuilder, "HUF_mergeIndex1", new()
        {
            {"bitC", new(
            [
                new("bitContainer.e0", true),
                new("bitPos.e0", true),
                new("bitContainer.e1", true),
                new("bitPos.e1", true),
            ])},
        }, true),
        new(projectBuilder, "HUF_flushBits", new()
        {
            {"bitC", new(
            [
                new("bitContainer.e0", true),
                new("bitPos.e0", true),
                new("ptr", true),
                new("endPtr"),
            ])},
        }, true),
        new(projectBuilder, "HUF_closeCStream", new()
        {
            {"bitC", RefMethodInfo.EmptyParameterInfo}
        }, true),
        new RefMethodInfoWithIdx(projectBuilder, "HUF_addBits", new()
        {
            {"bitC", new(
            [
                new("bitContainer.e0", true),
                new("bitPos.e0", true),
            ])},
            { "idx", null }
        }, "bitC", true),
        new RefMethodInfoWithIdx(projectBuilder, "HUF_encodeSymbol", new()
        {
            {"bitCPtr", new(
            [
                new("bitContainer.e0", true),
                new("bitPos.e0", true),
            ])},
            { "idx", null }
        }, "bitCPtr", true),
        new RefMethodInfoHufCompress1XUsingCTableInternalBodyLoop(projectBuilder, "HUF_compress1X_usingCTable_internal_body_loop", new ()
        {
            {"bitC", RefMethodInfo.EmptyParameterInfo}
        }, true),
        new(projectBuilder, "FSE_initDState", new ()
        {
            {"DStatePtr", RefMethodInfo.EmptyParameterInfo},
            {"bitD", RefMethodInfo.EmptyParameterInfo}
        }, true),
        new(projectBuilder, "FSE_decodeSymbol", new ()
        {
            {"DStatePtr", RefMethodInfo.EmptyParameterInfo},
            {"bitD", new(
            [
                new("bitContainer"),
                new("bitsConsumed", true),
            ])}
        }, true),
        new(projectBuilder, "FSE_decodeSymbolFast", new ()
        {
            {"DStatePtr", RefMethodInfo.EmptyParameterInfo},
            {"bitD", new(
            [
                new("bitContainer"),
                new("bitsConsumed", true),
            ])}
        }, true),
    ];

    public IReadOnlyList<RefMethodInfo> Methods => _methods;

    public record RefMethodParameterInfo(IReadOnlyList<VariableContext.FieldInfo>? InnerFields = null);

    public class RefMethodInfo
    {
        public readonly MethodDeclarationSyntax Method;
        public readonly FileBuilder Builder;
        public readonly string MethodName;
        public readonly Dictionary<string, RefMethodParameterInfo?> Parameters;
        public readonly bool Replace;
        public static readonly RefMethodParameterInfo EmptyParameterInfo = new();
        protected readonly ProjectBuilder ProjectBuilder;

        public RefMethodInfo(ProjectBuilder projectBuilder, string methodName,
            Dictionary<string, RefMethodParameterInfo?> parameters, bool replace = false)
        {
            if (!projectBuilder.TryGetMethod(methodName, out var fileBuilder, out var method))
            {
                throw new Exception($"Unknown method {methodName}");
            }

            ProjectBuilder = projectBuilder;
            Parameters = parameters;
            Replace = replace;
            Builder = fileBuilder; 
            Method = method;
            MethodName = methodName;

            foreach (var p in parameters.Where(paramInfo =>
                         Method.ParameterList.Parameters.All(p => paramInfo.Key != p.Identifier.ToString())))
            {
                projectBuilder.Reporter.Report(DiagnosticLevel.Warning,
                    $"Method {MethodName}, not found parameter {p.Key}");
            }
        }

        public Dictionary<ParameterSyntax, List<ParameterSyntax>>? GetParameterListReplacements(ParameterListSyntax node)
        {
            Dictionary<ParameterSyntax, List<ParameterSyntax>>? replacements = null;

            foreach (var p in node.Parameters)
            {
                if (Parameters.TryGetValue(p.Identifier.ToString(), out var parameterInfo))
                {
                    replacements ??= [];
                    if (parameterInfo == null)
                    {
                        replacements.Add(p, []);
                        continue;
                    }

                    var type = p.Type;
                    if (type is not PointerTypeSyntax pointerType)
                    {
                        throw new Exception($"Invalid type for argument {p.Identifier}");
                    }

                    if (parameterInfo.InnerFields == null)
                    {
                        replacements.Add(p,
                            [p.WithType(pointerType.ElementType).AddModifiers(SyntaxFactory.Token(SyntaxKind.RefKeyword))]);
                    }
                    else
                    {
                        var typeContext = new TypeContext(ProjectBuilder);
                        typeContext.AddStructType(pointerType.ElementType);

                        replacements.Add(p, [.. parameterInfo.InnerFields.Select(f =>
                        {
                            var fieldType = typeContext.GetFieldType(f.SplitPath, pointerType.ElementType);
                            var parameter = SyntaxFactory.Parameter(SyntaxFactory.Identifier($"{p.Identifier}_{f.VariableName}"))
                                .WithType(fieldType);

                            if (f.IsRef)
                            {
                                parameter = parameter.AddModifiers(SyntaxFactory.Token(SyntaxKind.RefKeyword));
                            }

                            return parameter;
                        })]);
                    }
                }
            }

            return replacements;
        }

        protected Dictionary<ArgumentSyntax, List<ArgumentSyntax>>? GetArgumentListReplacementsInternal(
            ArgumentListSyntax node, VariableContext variableContext, Dictionary<string, RefMethodParameterInfo?> parameters, bool dereference = false)
        {
            Dictionary<ArgumentSyntax, List<ArgumentSyntax>>? replacements = null;
            var index = 0;
            foreach (var arg in node.Arguments)
            {
                var innerExpression = TreeHelper.GetInnerExpression(arg.Expression);

                if (dereference)
                {
                    if (arg.Expression is PrefixUnaryExpressionSyntax prefixUnary &&
                        prefixUnary.Kind() == SyntaxKind.AddressOfExpression)
                    {
                        innerExpression = prefixUnary.Operand;
                    }
                }

                var parameterSyntax = Method.ParameterList.Parameters[index];
                if (parameters.TryGetValue(parameterSyntax.Identifier.ToString(), out var parameterInfo))
                {
                    replacements ??= [];
                    if (parameterInfo == null)
                    {
                        replacements.Add(arg, []);
                    }
                    else
                    {
                        // e -> ref e or [ref] e.field1, [ref] e.field2, 
                        if (parameterInfo.InnerFields == null)
                        {
                            replacements.Add(arg, [
                                arg.WithExpression(SyntaxFactory.RefExpression(SyntaxFactory.Token(SyntaxKind.RefKeyword), innerExpression))
                            ]);
                        }
                        else
                        {
                            replacements.Add(arg, [.. parameterInfo.InnerFields.Select(f =>
                            {
                                var argument = SyntaxFactory.Argument(variableContext.Resolve(f.GetMemberAccess(innerExpression)));
                                if (f.IsRef)
                                {
                                    argument = argument.WithRefOrOutKeyword(
                                        SyntaxFactory.Token(SyntaxKind.RefKeyword));
                                }
                                return argument;
                            })]);
                        }
                    }
                }

                index++;
            }

            return replacements;
        }

        public virtual Dictionary<ArgumentSyntax, List<ArgumentSyntax>>? GetArgumentListReplacements(ArgumentListSyntax node, VariableContext variableContext, bool dereference = false) 
            => GetArgumentListReplacementsInternal(node, variableContext, Parameters, dereference);

        public virtual VariableContext GetVariableContext()
        {
            var variableContext = new VariableContext();
            foreach (var (key, v) in Parameters)
            {
                if (v != null)
                {
                    if (v.InnerFields == null)
                    {
                        variableContext.Add(key);
                    }
                    else
                    {
                        variableContext.UnpackStruct(key, v.InnerFields);
                    }
                }
            }

            return variableContext;
        }
    }

    private class RefMethodInfoWithIdx : RefMethodInfo
    {
        private readonly string _bitCName;
        private readonly int _idxIndex;

        public RefMethodInfoWithIdx(ProjectBuilder projectBuilder, string methodName,
            Dictionary<string, RefMethodParameterInfo?> parameters, string bitCName, bool replace = false) : 
            base(projectBuilder, methodName, parameters, replace)
        {
            _bitCName = bitCName;
            _idxIndex = Method.ParameterList.Parameters.IndexOf(p => p.Identifier.ToString() == "idx");
        }

        public override VariableContext GetVariableContext()
        {
            var variableContext = base.GetVariableContext();
            // inherit idx from caller
            variableContext.Add("idx");
            return variableContext;
        }

        private Dictionary<string, RefMethodParameterInfo?> GetParametersForIdx(int idx)
        {
            var parameters = new Dictionary<string, RefMethodParameterInfo?>(Parameters);
            parameters[_bitCName] = new([.. parameters[_bitCName]!.InnerFields!.Select(f => f with {Path = f.Path.Replace(".e0", $".e{idx}")})]);
            return parameters;
        }

        public override Dictionary<ArgumentSyntax, List<ArgumentSyntax>>? GetArgumentListReplacements(ArgumentListSyntax node, VariableContext variableContext, bool dereference = false)
        {
            var idxExpression = node.Arguments[_idxIndex].Expression;
            var innerValue = TreeHelper.GetValueOfType<int>(idxExpression);
            if (innerValue == null)
            {
                // inherit idx
                if (variableContext.Contains("idx"))
                {
                    return base.GetArgumentListReplacements(node, variableContext, dereference);
                }

                throw new Exception($"Unknown argument idx value: {idxExpression}");
            }

            return GetArgumentListReplacementsInternal(node, variableContext, GetParametersForIdx(innerValue.Value), dereference);
        }
    }

    public class RefMethodInfoHufCompress1XUsingCTableInternalBodyLoop(
        ProjectBuilder projectBuilder,
        string methodName,
        Dictionary<string, RefMethodParameterInfo?> parameters,
        bool replace = false)
        : RefMethodInfo(projectBuilder, methodName, parameters, replace)
    {
        public static IReadOnlyList<ExtractStructModifier.VariableFields> GetExtractStructVariables()
        {
            return
            [
                new("bitC", SyntaxFactory.ParseTypeName("HUF_CStream_t"), [
                    new("ptr"),
                    new("endPtr"),
                    new("bitContainer.e0"),
                    new("bitPos.e0"),
                    new("bitContainer.e1"),
                    new("bitPos.e1"),
                ]),
            ];
        }

        public override VariableContext GetVariableContext()
        {
            var variableContext = base.GetVariableContext();
            variableContext.UnpackStructs(GetExtractStructVariables());
            return variableContext;
        }
    }

    public void Run()
    {
        foreach (var refMethod in _methods)
        {
            var modifier = refMethod.MethodName switch
            {
                // special modifier
                "HUF_addBits" => new RefMethodModifierForHufAddBits(reporter, refMethod, _methods),
                _ => new RefMethodModifier(reporter, refMethod, _methods)
            };

            var newMethod = modifier.Run(refMethod.Method);
            if (refMethod.Replace)
            {
                refMethod.Builder.ReplaceMethod(refMethod.MethodName, newMethod);
            }
            else
            {
                refMethod.Builder.AddMethodWithName(newMethod, refMethod.MethodName + "_ref");
            }
        }
    }
}

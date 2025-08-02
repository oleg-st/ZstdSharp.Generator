using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using ZstdSharp.Generator.CodeGenerator;

namespace ZstdSharp.Generator.Modify;

internal class ImproveFseDecompress(
    ProjectBuilder projectBuilder,
    IReadOnlyList<RefMethods.RefMethodInfo> refMethods)
{
    public void Run()
    {
        projectBuilder.ModifyMethod("FSE_decompress_usingDTable_generic", (_, method) =>
        {
            method = ProjectModifier.AddSkipInit(method, ["bitD", "state1", "state2"]);
            // first call to BIT_reloadDStream
            var firstCall = method.Body!.DescendantNodes().OfType<InvocationExpressionSyntax>().FirstOrDefault(e =>
                                e.Expression is IdentifierNameSyntax identifierName &&
                                identifierName.Identifier.ToString() == "BIT_reloadDStream") ??
                            throw new Exception($"Cannot find BIT_reloadDStream in {method.Identifier}");
            var topStatement = TreeHelper.GetTopStatement(method, firstCall) ??
                               throw new Exception($"Cannot find top statement in {method.Identifier}");
            var typeContext = new TypeContext(projectBuilder);
            var variableType = SyntaxFactory.ParseTypeName("BIT_DStream_t");
            typeContext.AddStructType(variableType);

            IReadOnlyList<ExtractStructModifier.VariableFields> variableFields =
            [
                new("bitD", variableType, [
                    new("bitContainer"),
                    new("bitsConsumed"),
                    new("ptr"),
                    new("start"),
                    new("limitPtr")
                ]),
            ];
            var variableContext = new VariableContext();
            variableContext.UnpackStructs(variableFields);

            method = new ExtractStructModifier(typeContext, variableFields, topStatement).Run(method);
            return new RefMethodUsageModifier(refMethods, variableContext).Run(method);
        });
    }
}

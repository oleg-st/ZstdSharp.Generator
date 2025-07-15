using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using ZstdSharp.Generator.CodeGenerator;

namespace ZstdSharp.Generator.Modify;

internal class ImproveCompressSequences(
    ProjectBuilder projectBuilder,
    IReadOnlyList<RefMethods.RefMethodInfo> refMethods)
{
    public void Run()
    {
        projectBuilder.ModifyMethod("ZSTD_encodeSequences_body", (_, method) =>
        {
            method = ProjectModifier.AddSkipInit(method, ["blockStream", "stateMatchLength", "stateOffsetBits", "stateLitLength"]);

            // first call to FSE_initCState2
            var firstCall = method.Body!.DescendantNodes().OfType<InvocationExpressionSyntax>().FirstOrDefault(e =>
                                e.Expression is IdentifierNameSyntax identifierName &&
                                identifierName.Identifier.ToString() == "FSE_initCState2") ??
                            throw new Exception($"Cannot find FSE_initCState2 in {method.Identifier}");
            var topStatement = TreeHelper.GetTopStatement(method, firstCall) ?? throw new Exception($"Cannot find top statement in {method.Identifier}");
            var typeContext = new TypeContext(projectBuilder);
            var variableType = SyntaxFactory.ParseTypeName("BIT_CStream_t");
            typeContext.AddStructType(variableType);

            IReadOnlyList<ExtractStructModifier.VariableFields> variableFields =
            [
                new("blockStream", variableType, [
                    new("bitContainer"),
                    new("bitPos"),
                    new("ptr"),
                    new("endPtr")
                ]),
            ];
            var variableContext = new VariableContext();
            variableContext.UnpackStructs(variableFields);

            method = new ExtractStructModifier(typeContext, variableFields, topStatement).Run(method);
            return new RefMethodUsageModifier(refMethods, variableContext).Run(method);
        });
    }
}

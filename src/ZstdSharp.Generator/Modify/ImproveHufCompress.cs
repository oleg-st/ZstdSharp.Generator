using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using ZstdSharp.Generator.CodeGenerator;

namespace ZstdSharp.Generator.Modify;

internal class ImproveHufCompress(
    ProjectBuilder projectBuilder,
    IReadOnlyList<RefMethods.RefMethodInfo> refMethods)
{
    public void Run()
    {
        projectBuilder.ModifyMethod("HUF_compress1X_usingCTable_internal_body_loop", (_, method) =>
        {
            const string bitC = "bitC";

            // HUF_CStream_t* bitC -> ref HUF_CStream_t bitC
            method = method.WithParameterList(method.ParameterList.WithParameters(
                SyntaxFactory.SeparatedList(method.ParameterList.Parameters.Select(
                    p => p.Identifier.ToString() ==
                        bitC && p.Type is PointerTypeSyntax pointerType
                            ? p.WithType(pointerType.ElementType)
                                .AddModifiers(SyntaxFactory.Token(SyntaxKind.RefKeyword))
                            : p))));

            // extract struct
            var topStatement = method.Body!.Statements.FirstOrDefault() ?? throw new Exception($"Cannot find top statement in {method.Identifier}");
            if (method.Body.DescendantNodes().OfType<ReturnStatementSyntax>().Any())
            {
                throw new Exception($"Return found in {method.Identifier} - not supported");
            }

            var typeContext = new TypeContext(projectBuilder);
            var variableType = SyntaxFactory.ParseTypeName("HUF_CStream_t");
            typeContext.AddStructType(variableType);

            var variableFields = RefMethods.RefMethodInfoHufCompress1XUsingCTableInternalBodyLoop.GetExtractStructVariables();
            method = new ExtractStructModifier(typeContext, variableFields, topStatement).Run(method);

            // restore struct
            var lastStatement = method.Body!.Statements.Last();
            return new RestoreStructModifier(variableFields, lastStatement).Run(method);
        });

        projectBuilder.ModifyMethod("HUF_compress1X_usingCTable_internal_body", (_, method) =>
        {
            method = method
                .WithAttributeLists(SyntaxFactory.List(method.AttributeLists.Where(attributeList =>
                    !attributeList.Attributes.Any(attribute =>
                        attribute.ToString().Contains("MethodImplOptions.AggressiveInlining")))));

            const string bitC = "bitC";
            method = ProjectModifier.AddSkipInit(method, [bitC]);
            var variableContext = new VariableContext();
            return new RefMethodUsageModifier(refMethods, variableContext).Run(method);
        });
    }
}

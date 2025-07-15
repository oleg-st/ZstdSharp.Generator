using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator;

namespace ZstdSharp.Generator.Modify;

internal class TypeContext(ProjectBuilder projectBuilder)
{
    private readonly Dictionary<string, TypeSyntax> _fieldTypes = [];

    private void ExtractStructTypes(StructDeclarationSyntax structDeclaration)
    {
        foreach (var member in structDeclaration.Members)
        {
            if (member is FieldDeclarationSyntax f)
            {
                foreach (var v in f.Declaration.Variables)
                {
                    _fieldTypes[$"{structDeclaration.Identifier}.{v.Identifier}"] = f.Declaration.Type;
                }
            }
            else if (member is StructDeclarationSyntax s)
            {
                ExtractStructTypes(s);
            }
        }
    }

    public void AddStructType(TypeSyntax type)
    {
        if (projectBuilder.TryGetTypeDeclaration(type.ToString(), out var typeDeclaration, out _) &&
            typeDeclaration is StructDeclarationSyntax structDeclaration)
        {
            ExtractStructTypes(structDeclaration);
        }
        else
        {
            throw new Exception($"Cannot determine field types for {type}");
        }
    }

    public TypeSyntax GetFieldType(IEnumerable<string> names, TypeSyntax fieldType)
    {
        foreach (var part in names)
        {
            if (!_fieldTypes.TryGetValue($"{fieldType}.{part}", out var innerType))
            {
                throw new Exception($"Unknown field type {part} in {fieldType}");
            }

            fieldType = innerType;
        }

        return fieldType;
    }
}

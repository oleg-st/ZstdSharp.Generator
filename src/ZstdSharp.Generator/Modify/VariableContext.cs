using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator;

namespace ZstdSharp.Generator.Modify;

internal class VariableContext
{
    private readonly Dictionary<string, string> _mapping = [];
    private readonly HashSet<string> _originalNames = [];

    public record FieldInfo(string Path, bool IsRef = false)
    {
        public string VariableName => Path.Replace('.', '_');

        public IEnumerable<string> SplitPath => Path.Split('.');

        public MemberAccessExpressionSyntax GetMemberAccess(ExpressionSyntax expression)
        {
            var currentNode = expression;
            foreach (var part in SplitPath)
            {
                currentNode = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, currentNode,
                    SyntaxFactory.IdentifierName(part));
            }

            return (MemberAccessExpressionSyntax) currentNode;
        }
    }

    public void Add(string name)
    {
        _mapping.Add(name, name);
        _originalNames.Add(name);
    }

    public void UnpackStruct(string name, IReadOnlyList<FieldInfo> fields) 
        => UnpackStruct(name, name, fields);

    public void UnpackStruct(string path, string variableName, IReadOnlyList<FieldInfo> fields)
    {
        _originalNames.Add(path);
        foreach (var field in fields)
        {
            _mapping.Add($"{path}.{field.Path}", $"{variableName}_{field.VariableName}");
        }
    }

    public void UnpackStructs(IEnumerable<ExtractStructModifier.VariableFields> variableFields)
    {
        foreach (var v in variableFields)
        {
            UnpackStruct(v.Path, v.VariableName, v.Fields);
        }
    }

    public string? Get(string name) 
        => _mapping.GetValueOrDefault(name);

    public bool Contains(string name)
        => _mapping.ContainsKey(name);

    public bool ContainsOriginal(string name) 
        => _originalNames.Contains(name);

    public static string? GetPath(ExpressionSyntax node)
    {
        var innerExpression = TreeHelper.GetInnerExpression(node);
        if (innerExpression is IdentifierNameSyntax identifierName)
        {
            return identifierName.ToString();
        }

        if (innerExpression is MemberAccessExpressionSyntax innerNode)
        {
            var innerPath = GetPath(innerNode.Expression);
            if (innerPath == null)
            {
                return null;
            }

            return $"{innerPath}.{innerNode.Name}";
        }

        return null;
    }

    public ExpressionSyntax Resolve(MemberAccessExpressionSyntax node)
    {
        var path = GetPath(node.Expression);
        if (path != null)
        {
            var key = $"{path}.{node.Name}";

            var value = Get(key);
            if (value != null)
            {
                return SyntaxFactory.IdentifierName(value);
            }
        }
        return node;
    }
}

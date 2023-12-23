using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ClangSharp;
using ClangSharp.Interop;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator.Extensions;
using Type = ClangSharp.Type;

namespace ZstdSharp.Generator.CodeGenerator;

// reused from PInvokeGenerator
internal partial class CodeGenerator
{
    private string GetRemappedName(string name)
    {
        if (Config.RemappedNames.TryGetValue(name, out var remappedName))
        {
            return remappedName;
        }

        if (name.StartsWith("const ") && Config.RemappedNames.TryGetValue(name[6..], out remappedName))
        {
            return remappedName;
        }

        return name;
    }

    private string GetAnonymousName(Cursor cursor, string kind)
    {
        cursor.Location.GetFileLocation(out var file, out var line, out var column, out _);
        var fileName = Path.GetFileNameWithoutExtension(file.Name.ToString());
        return $"__Anonymous{kind}_{fileName}_L{line}_C{column}";
    }

    private void WriteConstantArrayType(Cursor cursor, string name, ConstantArrayType constantArrayType)
    {
        if (!_projectBuilder.AddGeneratedType(name))
        {
            return;
        }

        StartFile(name);

        var escapedName = EscapeName(name);
        // todo
        var elementTypeName = GetCSharpType(cursor, constantArrayType.ElementType, out _).ToString();
        var structDeclarationSyntax = SyntaxFactory.StructDeclaration(escapedName)
            .WithModifiers(
                SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.UnsafeKeyword)));

        var elementType = GetType(elementTypeName);
        if (IsSupportedFixedSizedBufferType(elementTypeName))
        {
            var fieldDeclarationSyntax = SyntaxFactory.FieldDeclaration(
                    SyntaxFactory.VariableDeclaration(elementType)
                        .WithVariables(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(
                                        SyntaxFactory.Identifier("Body"))
                                    .WithArgumentList(
                                        SyntaxFactory.BracketedArgumentList(
                                            SyntaxFactory.SingletonSeparatedList(
                                                SyntaxFactory.Argument(
                                                    SyntaxFactory.LiteralExpression(
                                                        SyntaxKind.NumericLiteralExpression,
                                                        SyntaxFactory.Literal((int)constantArrayType.Size)))))))))
                .WithModifiers(
                    SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.FixedKeyword)));

            structDeclarationSyntax = structDeclarationSyntax.AddMembers(fieldDeclarationSyntax);
        }
        else
        {
            var totalSize = Math.Max(constantArrayType.Size, 1);

            for (long i = 0; i < totalSize; i++)
            {
                var fieldDeclarationSyntax = SyntaxFactory.FieldDeclaration(
                    SyntaxFactory.VariableDeclaration(
                            elementType)
                        .WithVariables(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier($"e{i}")))));

                structDeclarationSyntax = structDeclarationSyntax.AddMembers(fieldDeclarationSyntax);
            }
        }

        AddMember(structDeclarationSyntax);

        StopFile();
    }

    private string GetCursorQualifiedName(NamedDecl namedDecl, bool truncateFunctionParameters = false)
    {
        var parts = new Stack<NamedDecl>();

        for (Decl decl = namedDecl; decl.DeclContext != null; decl = (Decl) decl.DeclContext)
        {
            if (decl is NamedDecl innerNamedDecl)
            {
                parts.Push(innerNamedDecl);
            }
        }

        var qualifiedName = new StringBuilder();
        var part = parts.Pop();

        while (parts.Count != 0)
        {
            AppendNamedDecl(part, GetCursorName(part), qualifiedName);
            qualifiedName.Append('.');
            part = parts.Pop();
        }

        AppendNamedDecl(part, GetCursorName(part), qualifiedName);

        return qualifiedName.ToString();

        void AppendFunctionParameters(CXType functionType, StringBuilder innerQualifiedName)
        {
            if (truncateFunctionParameters)
            {
                return;
            }

            innerQualifiedName.Append('(');

            if (functionType.NumArgTypes != 0)
            {
                innerQualifiedName.Append(functionType.GetArgType(0).Spelling);

                for (uint i = 1; i < functionType.NumArgTypes; i++)
                {
                    innerQualifiedName.Append(',');
                    innerQualifiedName.Append(' ');
                    innerQualifiedName.Append(functionType.GetArgType(i).Spelling);
                }
            }

            innerQualifiedName.Append(')');
            innerQualifiedName.Append(':');

            innerQualifiedName.Append(functionType.ResultType.Spelling);

            if (functionType.ExceptionSpecificationType ==
                CXCursor_ExceptionSpecificationKind.CXCursor_ExceptionSpecificationKind_NoThrow)
            {
                innerQualifiedName.Append(' ');
                innerQualifiedName.Append("nothrow");
            }
        }

        void AppendNamedDecl(NamedDecl innerNamedDecl, string name, StringBuilder innerQualifiedName)
        {
            innerQualifiedName.Append(name);

            if (innerNamedDecl is FunctionDecl functionDecl)
            {
                AppendFunctionParameters(functionDecl.Type.Handle, innerQualifiedName);
            }
        }
    }

    private string GetRemappedCursorName(NamedDecl namedDecl)
    {
        var name = GetCursorQualifiedName(namedDecl);
        var remappedName = GetRemappedName(name);

        if (remappedName != name)
        {
            return remappedName;
        }

        name = GetCursorQualifiedName(namedDecl, truncateFunctionParameters: true);
        remappedName = GetRemappedName(name);

        if (remappedName != name)
        {
            return remappedName;
        }

        name = GetCursorName(namedDecl);
        remappedName = GetRemappedName(name);

        if (remappedName != name)
        {
            return remappedName;
        }

        if (namedDecl is FieldDecl fieldDecl && name.StartsWith("__AnonymousField_"))
        {
            remappedName = name;

            if (fieldDecl.Parent?.AnonymousFields.Count > 1)
            {
                var index = fieldDecl.Parent.AnonymousFields.IndexOf(fieldDecl) + 1;
                remappedName += index.ToString();
            }
        }
        else if (namedDecl is RecordDecl recordDecl && name.StartsWith("__AnonymousRecord_"))
        {
            remappedName = name;

            if (recordDecl.Parent is RecordDecl parentRecordDecl)
            {
                var matchingField = parentRecordDecl.Fields
                    .FirstOrDefault(decl => decl.Type.CanonicalType == recordDecl.TypeForDecl.CanonicalType);

                if (matchingField != null)
                {
                    remappedName = "_";
                    remappedName += GetRemappedCursorName(matchingField);
                }
                else if (parentRecordDecl.AnonymousRecords.Count > 1)
                {
                    var index = parentRecordDecl.AnonymousRecords.IndexOf(recordDecl) + 1;
                    remappedName += index.ToString();
                }
            }

            remappedName += $"_e__{(recordDecl.IsUnion ? "Union" : "Struct")}";
        }

        return remappedName;
    }

    private string EscapeName(string name)
    {
        return name is "abstract" or "as" or "base" or "bool" or "break" or "byte" or "case" or "catch" or "char"
            or "checked" or "class" or "const" or "continue" or "decimal" or "default" or "delegate" or "do" or "double"
            or "else" or "enum" or "event" or "explicit" or "extern" or "false" or "finally" or "fixed" or "float"
            or "for" or "foreach" or "goto" or "if" or "implicit" or "in" or "int" or "interface" or "internal" or "is"
            or "lock" or "long" or "namespace" or "new" or "null" or "object" or "operator" or "out" or "override"
            or "params" or "private" or "protected" or "public" or "readonly" or "ref" or "return" or "sbyte"
            or "sealed" or "short" or "sizeof" or "stackalloc" or "static" or "string" or "struct" or "switch" or "this"
            or "throw" or "true" or "try" or "typeof" or "uint" or "ulong" or "unchecked" or "unsafe" or "ushort"
            or "using" or "using static" or "virtual" or "void" or "volatile" or "while" ? $"@{name}" : name;
    }

    private string GetCursorName(NamedDecl namedDecl)
    {
        var name = namedDecl.Name.Replace('\\', '/');

        if (string.IsNullOrWhiteSpace(name) || name.Contains('('))
        {
            if (namedDecl is TypeDecl typeDecl)
            {
                if (typeDecl is TagDecl {Handle.IsAnonymous: true} tagDecl)
                {
                    name = GetAnonymousName(tagDecl, tagDecl.TypeForDecl.KindSpelling);
                }
                else
                {
                    // todo
                    name = GetCSharpType(namedDecl, typeDecl.TypeForDecl, out _).ToString();
                }
            }
            else if (namedDecl is ParmVarDecl)
            {
                name = "param";
            }
            else if (namedDecl is FieldDecl fieldDecl)
            {
                name = GetAnonymousName(fieldDecl, fieldDecl.CursorKindSpelling);
            }
            else
            {
                Report(DiagnosticLevel.Error, $"Unsupported anonymous named declaration: {namedDecl.Kind}");
            }
        }
        else if (namedDecl is CXXDestructorDecl)
        {
            name = "Finalize";
        }

        if (string.IsNullOrWhiteSpace(name))
        {
            Report(DiagnosticLevel.Error, "Unknown cursor name");
        }
        return name;
    }

    private string ConvertName(string name)
    {
        var stringBuilder = new StringBuilder(name.Length);
        var index = 0;
        while (index < name.Length)
        {
            var toIndex = name.IndexOf('_', index);
            var part = toIndex >= 0 ? name.Substring(index, toIndex - index) : name.Substring(index);
            if (part.Length > 0)
            {
                stringBuilder.Append(char.ToUpperInvariant(part[0]));
                stringBuilder.Append(part[1..].ToLowerInvariant());
            }

            if (toIndex < 0)
            {
                break;
            }

            index = toIndex + 1;
        }

        return stringBuilder.ToString();
    }

    public string GetMethodsFileName(Cursor cursor)
    {
        cursor.Location.GetFileLocation(out var file, out _, out _, out _);
        var fileName = Path.GetFileNameWithoutExtension(file.Name.ToString());
        return ConvertName(fileName);
    }

    private bool IsExcluded(Cursor cursor)
    {
        cursor.Location.GetFileLocation(out _, out _, out _, out _);

        if (IsAlwaysIncluded(cursor))
        {
            return false;
        }

        return IsExcludedByFile(cursor) || IsExcludedByName(cursor);

        bool IsAlwaysIncluded(Cursor innerCursor)
        {
            return innerCursor is TranslationUnitDecl or LinkageSpecDecl;
        }

        bool IsExcludedByFile(Cursor innerCursor)
        {
            var declLocation = innerCursor.Location;
            declLocation.GetFileLocation(out CXFile file, out _, out _, out _);

            if (IsIncludedFileOrLocation(file, declLocation))
            {
                return false;
            }

            // It is not uncommon for some declarations to be done using macros, which are themselves
            // defined in an imported header file. We want to also check if the expansion location is
            // in the main file to catch these cases and ensure we still generate bindings for them.

            declLocation.GetExpansionLocation(out CXFile expansionFile, out uint line, out uint column, out _);
            if (expansionFile == file)
            {
                // clang_getLocation is a very expensive call, so exit early if the expansion file is the same
                return true;
            }

            var expansionLocation = innerCursor.TranslationUnit.Handle.GetLocation(file, line, column);
            return !IsIncludedFileOrLocation(file, expansionLocation);
        }

        bool IsExcludedByName(Cursor innerCursor)
        {
            string qualifiedName, name;

            if (innerCursor is NamedDecl namedDecl)
            {
                if (namedDecl is TagDecl tagDecl && tagDecl.Definition != tagDecl && tagDecl.Definition != null)
                {
                    // We don't want to generate bindings for anything
                    // that is not itself a definition and that has a
                    // definition that can be resolved. This ensures we
                    // still generate bindings for things which are used
                    // as opaque handles, but which aren't ever defined.

                    return true;
                }

                // We get the non-remapped name for the purpose of exclusion checks to ensure that users
                // can remove no-definition declarations in favor of remapped anonymous declarations.

                qualifiedName = GetCursorQualifiedName(namedDecl);
                name = GetCursorName(namedDecl);
            }
            else if (innerCursor is MacroDefinitionRecord macroDefinitionRecord)
            {
                qualifiedName = macroDefinitionRecord.Name;
                name = macroDefinitionRecord.Name;
            }
            else
            {
                return false;
            }

            return Config.ExcludedNames.Contains(qualifiedName) || Config.ExcludedNames.Contains(name);
        }

        bool IsIncludedFileOrLocation(CXFile file, CXSourceLocation location)
        {
            // Use case insensitive comparison on Windows
            var equalityComparer = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
                ? StringComparer.OrdinalIgnoreCase
                : StringComparer.Ordinal;

            // Normalize paths to be '/' for comparison
            var fileName = file.Name.ToString();
            if (string.IsNullOrEmpty(fileName))
            {
                return false;
            }

            fileName = Path.GetFullPath(fileName).Replace('\\', '/');

            if (Config.TraversalNames.Contains(fileName, equalityComparer))
            {
                return true;
            }

            if (Config.TraversalNames.Count == 0 && location.IsFromMainFile)
            {
                return true;
            }

            return false;
        }
    }

    public string GetTypeFileName(NamedDecl namedDecl) => GetRemappedCursorName(namedDecl);

    // todo
    private bool IsSupportedFixedSizedBufferType(string typeName) =>
        typeName is "bool" or "byte" or "char" or "double" or "float" or "int" or "long" or "sbyte" or "short"
            or "ushort" or "uint" or "ulong";

    private bool IsUnsafe(FieldDecl fieldDecl)
    {
        var type = fieldDecl.Type;

        if (type.CanonicalType is ConstantArrayType constantArrayType)
        {
            // todo
            var name = GetCSharpType(fieldDecl, constantArrayType.ElementType, out _).ToString();

            if (!Config.RemappedNames.TryGetValue(name, out var remappedName))
            {
                remappedName = name;
            }

            return IsSupportedFixedSizedBufferType(remappedName);
        }

        return IsUnsafe(fieldDecl, type);
    }

    private bool IsUnsafe(RecordDecl recordDecl)
    {
        foreach (var decl in recordDecl.Decls)
        {
            if ((decl is FieldDecl fieldDecl && IsUnsafe(fieldDecl)) ||
                (decl is RecordDecl {IsAnonymousStructOrUnion: true} nestedRecordDecl && IsUnsafe(nestedRecordDecl)))
                return true;
        }

        return false;
    }

    private bool IsUnsafe(NamedDecl namedDecl, Type type)
    {
        // todo
        var name = GetCSharpType(namedDecl, type, out _).ToString();

        if (!Config.RemappedNames.TryGetValue(name, out var remappedName))
        {
            remappedName = name;
        }

        return remappedName.Contains('*');
    }

    private bool IsUnsafe(TypedefDecl typedefDecl, FunctionProtoType functionProtoType)
    {
        var returnType = functionProtoType.ReturnType;

        if (IsUnsafe(typedefDecl, returnType))
        {
            return true;
        }

        foreach (var paramType in functionProtoType.ParamTypes)
        {
            if (IsUnsafe(typedefDecl, paramType))
            {
                return true;
            }
        }

        return false;
    }

    private bool IsVoidExpr(ExpressionSyntax expression)
    {
        return expression switch
        {
            CastExpressionSyntax castExpression when castExpression.Type.ToString() == "void" &&
                                                     IsPureExpr(castExpression.Expression) => true,
            BinaryExpressionSyntax binaryOperator => IsVoidExpr(binaryOperator.Left) && IsVoidExpr(binaryOperator.Right),
            PrefixUnaryExpressionSyntax prefixUnaryExpressionSyntax => IsVoidExpr(prefixUnaryExpressionSyntax.Operand),
            PostfixUnaryExpressionSyntax postfixUnaryExpressionSyntax => IsVoidExpr(postfixUnaryExpressionSyntax.Operand),
            ParenthesizedExpressionSyntax parenthesizedExpression => IsVoidExpr(parenthesizedExpression.Expression),
            CastExpressionSyntax castExpression2 => IsVoidExpr(castExpression2.Expression),
            _ => false
        };
    }

    private bool IsPureExpr(ExpressionSyntax expression)
    {
        return expression switch
        {
            BinaryExpressionSyntax binaryOperator => IsPureExpr(binaryOperator.Left) && IsPureExpr(binaryOperator.Right),
            PrefixUnaryExpressionSyntax unaryOperator => IsPureExpr(unaryOperator.Operand),
            PostfixUnaryExpressionSyntax postfixUnaryExpressionSyntax => IsPureExpr(postfixUnaryExpressionSyntax.Operand),
            ParenthesizedExpressionSyntax parenExpr => IsPureExpr(parenExpr.Expression),
            CastExpressionSyntax castExpr => IsPureExpr(castExpr.Expression),
            MemberAccessExpressionSyntax memberAccess => IsPureExpr(memberAccess.Expression),
            ElementAccessExpressionSyntax elementAccessExpression => IsPureExpr(elementAccessExpression.Expression),
            IdentifierNameSyntax => true,
            SizeOfExpressionSyntax => true,
            LiteralExpressionSyntax => true,
            _ => false,
        };
    }

    internal bool IsSizeOfConst(UnaryExprOrTypeTraitExpr unaryExprOrTypeTraitExpr)
    {
        var cSharpType = GetRemappedCSharpType(unaryExprOrTypeTraitExpr, unaryExprOrTypeTraitExpr.TypeOfArgument, out _, false);

        while (true)
        {
            if (cSharpType is ArrayTypeSyntax arrayType)
            {
                cSharpType = arrayType.ElementType;
                continue;
            }

            return cSharpType is PredefinedTypeSyntax;
        }
    }

    private bool IsConstExpr(Expr expr)
    {
        return expr switch
        {
            BinaryOperator binaryOperator => IsConstExpr(binaryOperator.LHS) && IsConstExpr(binaryOperator.RHS),
            UnaryOperator unaryOperator => IsConstExpr(unaryOperator.SubExpr),
            ParenExpr parenExpr => IsConstExpr(parenExpr.SubExpr),
            CastExpr castExpr => IsConstExpr(castExpr.SubExpr),
            MemberExpr memberExpr => IsConstExpr(memberExpr.Base),
            _ => expr is IntegerLiteral || (expr is UnaryExprOrTypeTraitExpr
            {
                Kind: CX_UnaryExprOrTypeTrait.CX_UETT_SizeOf
            } unaryExprOrTypeTraitExpr && IsSizeOfConst(unaryExprOrTypeTraitExpr))
        };
    }

    private static bool? GetConstantCond(Expr expr)
    {
        var canonicalType = expr.Type.CanonicalType;
        if (canonicalType.IsIntegerType && canonicalType.Kind != CXTypeKind.CXType_Bool)
        {
            if (expr is UnaryOperator {Opcode: CXUnaryOperatorKind.CXUnaryOperator_LNot, SubExpr: IntegerLiteral nIntegerLiteral})
            {
                return nIntegerLiteral.Value == 0;
            }

            if (expr is IntegerLiteral integerLiteral)
            {
                return integerLiteral.Value != 0;
            }
        }

        return null;
    }

    private string GetCallingConventionName(CXCallingConv callingConvention)
    {
        switch (callingConvention)
        {
            case CXCallingConv.CXCallingConv_C:
            {
                return "Cdecl";
            }

            case CXCallingConv.CXCallingConv_X86StdCall:
            {
                return "StdCall";
            }

            case CXCallingConv.CXCallingConv_X86FastCall:
            {
                return "FastCall";
            }

            case CXCallingConv.CXCallingConv_X86ThisCall:
            {
                return "ThisCall";
            }

            case CXCallingConv.CXCallingConv_Win64:
            {
                return "Winapi";
            }

            default:
            {
                var name = "Winapi";
                Report(DiagnosticLevel.Warning, $"Unsupported calling convention: {callingConvention}");
                return name;
            }
        }
    }

    private Expr GetExprAsWritten(Expr expr, bool removeParens)
    {
        do
        {
            if (expr is ImplicitCastExpr implicitCastExpr)
            {
                expr = implicitCastExpr.SubExprAsWritten;
            }
            else if (removeParens && expr is ParenExpr parenExpr)
            {
                expr = parenExpr.SubExpr;
            }
            else
            {
                return expr;
            }
        }
        while (true);
    }
}

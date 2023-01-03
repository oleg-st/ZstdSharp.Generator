using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;
using ClangSharp;
using ClangSharp.Interop;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator.Extensions;

namespace ZstdSharp.Generator.CodeGenerator;

internal partial class CodeGenerator
{
    private string GetArtificialFixedSizedBufferName(string name)
    {
        return $"_{name}_e__FixedBuffer";
    }

    private SyntaxList<AttributeListSyntax> GetInlineAttributes() =>
        SyntaxFactory.List(
            new[]
            {
                SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("MethodImpl"),
                            SyntaxFactory.ParseAttributeArgumentList("(MethodImplOptions.AggressiveInlining)"))
                    )),
                SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("InlineMethod.Inline"))
                    ))
            });

    private BaseTypeDeclarationSyntax CreateFixedBuffer(string name, TypeSyntax type, int size)
    {
        AddUsing("System.Runtime.CompilerServices");
        AddUsing("static ZstdSharp.UnsafeHelper");

        var fixedBufferName = GetArtificialFixedSizedBufferName(name);

        IEnumerable<MemberDeclarationSyntax> FieldsEnumerator()
        {
            for (var i = 0; i < size; i++)
            {
                yield return SyntaxFactory.FieldDeclaration(
                        SyntaxFactory.VariableDeclaration(
                                type)
                            .WithVariables(
                                SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.VariableDeclarator(
                                        SyntaxFactory.Identifier($"e{i}")))))
                    .WithModifiers(
                        SyntaxFactory.TokenList(
                            SyntaxFactory.Token(SyntaxKind.PublicKeyword)));
            }
        }

        var inlineAttributes = GetInlineAttributes();
        return SyntaxFactory.StructDeclaration(fixedBufferName)
            .WithModifiers(
                SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.UnsafeKeyword)))
            .WithMembers(
                SyntaxFactory.List(
                    FieldsEnumerator().Concat(
                        new MemberDeclarationSyntax[]
                        {
                            SyntaxFactory.IndexerDeclaration(
                                    SyntaxFactory.RefType(type))
                                .WithModifiers(
                                    SyntaxFactory.TokenList(
                                        SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                                .WithParameterList(
                                    SyntaxFactory.BracketedParameterList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.Parameter(
                                                    SyntaxFactory.Identifier("index"))
                                                .WithType(
                                                    SyntaxFactory.IdentifierName("nuint")))))
                                .WithAccessorList(
                                    SyntaxFactory.AccessorList(
                                        SyntaxFactory.SingletonList(
                                            SyntaxFactory.AccessorDeclaration(
                                                    SyntaxKind.GetAccessorDeclaration)
                                                .WithAttributeLists(
                                                    inlineAttributes)
                                                .WithExpressionBody(
                                                    SyntaxFactory.ArrowExpressionClause(
                                                        SyntaxFactory.RefExpression(
                                                            SyntaxFactory.PrefixUnaryExpression(
                                                                SyntaxKind.PointerIndirectionExpression,
                                                                SyntaxFactory.ParenthesizedExpression(
                                                                    SyntaxFactory.BinaryExpression(
                                                                        SyntaxKind.AddExpression,
                                                                        SyntaxFactory.InvocationExpression(
                                                                                SyntaxFactory.GenericName(
                                                                                        SyntaxFactory.Identifier(
                                                                                            "RefToPointer"))
                                                                                    .WithTypeArgumentList(
                                                                                        SyntaxFactory.TypeArgumentList(
                                                                                            SyntaxFactory.SeparatedList(
                                                                                                new[]
                                                                                                {
                                                                                                    SyntaxFactory
                                                                                                        .IdentifierName(
                                                                                                            fixedBufferName),
                                                                                                    type
                                                                                                }))))
                                                                            .WithArgumentList(
                                                                                SyntaxFactory.ArgumentList(
                                                                                    SyntaxFactory
                                                                                        .SingletonSeparatedList(
                                                                                            SyntaxFactory.Argument(
                                                                                                SyntaxFactory
                                                                                                    .ThisExpression())))),
                                                                        SyntaxFactory.IdentifierName("index")))))))
                                                .WithSemicolonToken(
                                                    SyntaxFactory.Token(SyntaxKind.SemicolonToken))))),
                            SyntaxFactory.IndexerDeclaration(
                                    SyntaxFactory.RefType(type))
                                .WithModifiers(
                                    SyntaxFactory.TokenList(
                                        SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                                .WithParameterList(
                                    SyntaxFactory.BracketedParameterList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.Parameter(
                                                    SyntaxFactory.Identifier("index"))
                                                .WithType(
                                                    SyntaxFactory.IdentifierName("nint")))))
                                .WithAccessorList(
                                    SyntaxFactory.AccessorList(
                                        SyntaxFactory.SingletonList(
                                            SyntaxFactory.AccessorDeclaration(
                                                    SyntaxKind.GetAccessorDeclaration)
                                                .WithAttributeLists(
                                                    inlineAttributes)
                                                .WithExpressionBody(
                                                    SyntaxFactory.ArrowExpressionClause(
                                                        SyntaxFactory.RefExpression(
                                                            SyntaxFactory.PrefixUnaryExpression(
                                                                SyntaxKind.PointerIndirectionExpression,
                                                                SyntaxFactory.ParenthesizedExpression(
                                                                    SyntaxFactory.BinaryExpression(
                                                                        SyntaxKind.AddExpression,
                                                                        SyntaxFactory.InvocationExpression(
                                                                                SyntaxFactory.GenericName(
                                                                                        SyntaxFactory.Identifier(
                                                                                            "RefToPointer"))
                                                                                    .WithTypeArgumentList(
                                                                                        SyntaxFactory.TypeArgumentList(
                                                                                            SyntaxFactory
                                                                                                .SeparatedList(new[]
                                                                                                {
                                                                                                    SyntaxFactory
                                                                                                        .IdentifierName(
                                                                                                            fixedBufferName),
                                                                                                    type
                                                                                                }))))
                                                                            .WithArgumentList(
                                                                                SyntaxFactory.ArgumentList(
                                                                                    SyntaxFactory
                                                                                        .SingletonSeparatedList(
                                                                                            SyntaxFactory.Argument(
                                                                                                SyntaxFactory
                                                                                                    .ThisExpression())))),
                                                                        SyntaxFactory.IdentifierName("index")))))))
                                                .WithSemicolonToken(
                                                    SyntaxFactory.Token(SyntaxKind.SemicolonToken))))),
                            SyntaxFactory.ConversionOperatorDeclaration(
                                    SyntaxFactory.Token(SyntaxKind.ImplicitKeyword),
                                    SyntaxFactory.PointerType(type))
                                .WithAttributeLists(inlineAttributes)
                                .WithModifiers(
                                    SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                                        SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
                                .WithParameterList(
                                    SyntaxFactory.ParameterList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.Parameter(
                                                    SyntaxFactory.Identifier("t"))
                                                .WithModifiers(
                                                    SyntaxFactory.TokenList(
                                                        SyntaxFactory.Token(SyntaxKind.InKeyword)))
                                                .WithType(
                                                    SyntaxFactory.IdentifierName(fixedBufferName)))))
                                .WithExpressionBody(
                                    SyntaxFactory.ArrowExpressionClause(
                                        SyntaxFactory.InvocationExpression(
                                                SyntaxFactory.GenericName(
                                                        SyntaxFactory.Identifier("RefToPointer"))
                                                    .WithTypeArgumentList(
                                                        SyntaxFactory.TypeArgumentList(
                                                            SyntaxFactory.SeparatedList(
                                                                new[]
                                                                {
                                                                    SyntaxFactory.IdentifierName(fixedBufferName),
                                                                    type
                                                                }))))
                                            .WithArgumentList(
                                                SyntaxFactory.ArgumentList(
                                                    SyntaxFactory.SingletonSeparatedList(
                                                        SyntaxFactory.Argument(
                                                            SyntaxFactory.IdentifierName("t")))))))
                                .WithSemicolonToken(
                                    SyntaxFactory.Token(SyntaxKind.SemicolonToken)),
                            SyntaxFactory.OperatorDeclaration(
                                    SyntaxFactory.PointerType(type),
                                    SyntaxFactory.Token(SyntaxKind.PlusToken))
                                .WithAttributeLists(inlineAttributes)
                                .WithModifiers(
                                    SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                                        SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
                                .WithParameterList(
                                    SyntaxFactory.ParameterList(
                                        SyntaxFactory.SeparatedList(
                                            new[]
                                            {
                                                SyntaxFactory.Parameter(
                                                        SyntaxFactory.Identifier("t"))
                                                    .WithModifiers(
                                                        SyntaxFactory.TokenList(
                                                            SyntaxFactory.Token(SyntaxKind.InKeyword)))
                                                    .WithType(
                                                        SyntaxFactory.IdentifierName(fixedBufferName)),
                                                SyntaxFactory.Parameter(
                                                        SyntaxFactory.Identifier("index"))
                                                    .WithType(
                                                        SyntaxFactory.IdentifierName("nuint"))
                                            })))
                                .WithExpressionBody(
                                    SyntaxFactory.ArrowExpressionClause(
                                        SyntaxFactory.BinaryExpression(
                                            SyntaxKind.AddExpression,
                                            SyntaxFactory.InvocationExpression(
                                                    SyntaxFactory.GenericName(
                                                            SyntaxFactory.Identifier("RefToPointer"))
                                                        .WithTypeArgumentList(
                                                            SyntaxFactory.TypeArgumentList(
                                                                SyntaxFactory.SeparatedList(
                                                                    new[]
                                                                    {
                                                                        SyntaxFactory.IdentifierName(fixedBufferName),
                                                                        type
                                                                    }))))
                                                .WithArgumentList(
                                                    SyntaxFactory.ArgumentList(
                                                        SyntaxFactory.SingletonSeparatedList(
                                                            SyntaxFactory.Argument(
                                                                SyntaxFactory.IdentifierName("t"))))),
                                            SyntaxFactory.IdentifierName("index"))))
                                .WithSemicolonToken(
                                    SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                        })));
    }

    private TypeSyntax GetCSharpTypeForPointeeType(Cursor cursor, Type pointeeType)
    {
        if (pointeeType is AttributedType attributedType)
        {
            return GetCSharpTypeForPointeeType(cursor, attributedType.ModifiedType);
        }

        if (pointeeType is FunctionType)
        {
            return GetType("IntPtr");
        }

        var cSharpType = GetCSharpType(cursor, pointeeType, out _, innerPointer: true);
        var remappedName = GetRemappedName(cSharpType.ToString());

        if (IsStructToClass(remappedName))
        {
            // dereference
            return GetType(remappedName);
        }

        return SyntaxFactory.PointerType(GetType(remappedName));
    }

    private TypeSyntax GetCSharpType(Cursor cursor, Type type, out bool arrayConvertedToPointer,
        int arrayLevel = 0,
        bool innerPointer = false,
        bool useTypeDef = false, bool canConvertArrayToPointer = true)
    {
        var name = new StringBuilder(type.AsString)
            .Replace('\\', '/')
            .Replace("struct ", "")
            .Replace("const ", "")
            .Replace("union ", "")
            .ToString();

        TypeSyntax cSharpType;
        arrayConvertedToPointer = false;

        switch (type)
        {
            case ArrayType arrayType:
                {
                    var elementType = GetCSharpType(cursor, arrayType.ElementType, out _, useTypeDef: true, canConvertArrayToPointer: false, arrayLevel: arrayLevel + 1);
                    var innerType = GetType(GetRemappedName(elementType.ToString()));

                    if (canConvertArrayToPointer && (cursor is FunctionDecl or ParmVarDecl || innerPointer ||
                                                     (arrayType.ElementType is not ArrayType &&
                                                      arrayType.ElementType.CanonicalType is not PointerType
                                                      {
                                                          PointeeType: FunctionProtoType
                                                      })))
                    {
                        arrayConvertedToPointer = true;
                        cSharpType = SyntaxFactory.PointerType(innerType);
                    }
                    else
                    {
                        // int[][] x -> int[,] x
                        if (Config.ConvertNestedArraysToMultidimensional && innerType is ArrayTypeSyntax {RankSpecifiers.Count: 1} arrayTypeSyntax)
                        {
                            cSharpType = SyntaxFactory.ArrayType(arrayTypeSyntax.ElementType)
                                .WithRankSpecifiers(
                                    SyntaxFactory.SingletonList(
                                        arrayTypeSyntax.RankSpecifiers[0].AddSizes(
                                            SyntaxFactory.OmittedArraySizeExpression()
                                        )));
                        }
                        else
                        {
                            cSharpType = SyntaxFactory.ArrayType(innerType)
                                .WithRankSpecifiers(
                                    SyntaxFactory.SingletonList(
                                        SyntaxFactory.ArrayRankSpecifier(
                                            SyntaxFactory.SingletonSeparatedList<ExpressionSyntax>(
                                                SyntaxFactory.OmittedArraySizeExpression()))));
                        }
                    }

                    break;
                }
            case AttributedType attributedType:
                cSharpType = GetCSharpType(cursor, attributedType.ModifiedType, out _);
                break;
            case BuiltinType:
                switch (type.Kind)
                {
                    case CXTypeKind.CXType_Void:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.VoidKeyword));
                        break;

                    case CXTypeKind.CXType_Bool:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.BoolKeyword));
                        break;

                    case CXTypeKind.CXType_Char_U:
                    case CXTypeKind.CXType_UChar:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.ByteKeyword));
                        break;

                    case CXTypeKind.CXType_Char16:
                    case CXTypeKind.CXType_WChar:
                    case CXTypeKind.CXType_UShort:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.UShortKeyword));
                        break;

                    case CXTypeKind.CXType_ULong:
                    case CXTypeKind.CXType_UInt:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.UIntKeyword));
                        break;

                    case CXTypeKind.CXType_ULongLong:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.ULongKeyword));
                        break;

                    case CXTypeKind.CXType_Char_S:
                    case CXTypeKind.CXType_SChar:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.SByteKeyword));
                        break;

                    case CXTypeKind.CXType_Short:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.ShortKeyword));
                        break;

                    case CXTypeKind.CXType_Int:
                    case CXTypeKind.CXType_Long:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.IntKeyword));
                        break;

                    case CXTypeKind.CXType_LongLong:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.LongKeyword));
                        break;

                    case CXTypeKind.CXType_Float:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.FloatKeyword));
                        break;

                    case CXTypeKind.CXType_Double:
                        cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.DoubleKeyword));
                        break;

                    case CXTypeKind.CXType_Unexposed:
                        // todo pseudo type ?
                        cSharpType = GetType("unexposed");
                        break;

                    default:
                        cSharpType = GetType(name);
                        Report(DiagnosticLevel.Warning,
                            $"Unsupported builtin type: {type.TypeClass} {type.KindSpelling}. Falling back {cSharpType}");
                        break;
                }

                break;
            case DeducedType deducedType:
                cSharpType = GetCSharpType(cursor, deducedType.CanonicalType, out _);
                break;
            case ElaboratedType elaboratedType:
                cSharpType = GetCSharpType(cursor, elaboratedType.NamedType, out _);
                break;
            case FunctionType functionType:
                cSharpType = GetCSharpTypeForPointeeType(cursor, functionType);
                break;
            case PointerType pointerType:
                cSharpType = GetCSharpTypeForPointeeType(cursor, pointerType.PointeeType);
                break;
            case ReferenceType referenceType:
                cSharpType = GetCSharpTypeForPointeeType(cursor, referenceType.PointeeType);
                break;
            case TagType { Decl.Handle.IsAnonymous: true } tagType:
                cSharpType = GetType(GetAnonymousName(tagType.Decl, tagType.KindSpelling));
                break;
            case TagType { Handle.IsConstQualified: true } tagType:
                cSharpType = GetCSharpType(cursor, tagType.Decl.TypeForDecl, out _);
                break;
            case TagType:
                cSharpType = GetType(name);
                break;
            case TypedefType typedefType:
                {
                    // We check remapped names here so that types that have variable sizes
                    // can be treated correctly. Otherwise, they will resolve to a particular
                    // platform size, based on whatever parameters were passed into clang.

                    var remappedName = GetRemappedName(name);

                    if (remappedName.Equals(name))
                    {
                        if (typedefType.Decl.UnderlyingType is PointerType { PointeeType: FunctionProtoType functionProtoType })
                        {
                            var useFunctionPointer = UseFunctionPointerForType(name);
                            if (useFunctionPointer && arrayLevel > 1)
                            {
                                _reporter.Report(DiagnosticLevel.Warning, $"Nested array with function pointer {name}");
                            }

                            cSharpType = useFunctionPointer ? GetFunctionPointerType(cursor, functionProtoType) : GetType(name);
                        }
                        else if ((innerPointer || useTypeDef) &&
                                 typedefType.Decl.UnderlyingType is ConstantArrayType constantArrayType)
                        {
                            cSharpType = GetType(name);
                            WriteConstantArrayType(cursor, name, constantArrayType);
                        }
                        else
                        {
                            cSharpType = GetCSharpType(cursor, typedefType.Decl.UnderlyingType, out _,
                                innerPointer: innerPointer);
                        }
                    }
                    else
                    {
                        cSharpType = GetType(remappedName);
                    }

                    break;
                }
            default:
                cSharpType = GetType(name);
                Report(DiagnosticLevel.Warning, $"Unsupported type: {type.TypeClass}. Falling back {cSharpType}");
                break;
        }

        return cSharpType;
    }

    internal TypeSyntax GetFunctionPointerType(Cursor typedefDecl, FunctionProtoType functionProtoType)
    {
        var paramList = functionProtoType.ParamTypes
            .Select(paramType => GetRemappedCSharpType(typedefDecl, paramType, out _))
            .Concat(new[] {GetRemappedCSharpType(typedefDecl, functionProtoType.ReturnType, out _)});

        return SyntaxFactory.FunctionPointerType()
            .WithCallingConvention(
                SyntaxFactory.FunctionPointerCallingConvention(
                    SyntaxFactory.Token(SyntaxKind.ManagedKeyword)))
            .WithParameterList(
                SyntaxFactory.FunctionPointerParameterList(
                    SyntaxFactory.SeparatedList(
                        paramList
                            .Select(SyntaxFactory.FunctionPointerParameter)
                    )));
    }

    internal TypeSyntax GetRemappedCSharpType(Cursor cursor, Type type, out bool arrayConvertedToPointer,
        bool canConvertArrayToPointer = true)
    {
        var cSharpType = GetCSharpType(cursor, type, out arrayConvertedToPointer, canConvertArrayToPointer: canConvertArrayToPointer);

        var name = GetRemappedName(cSharpType.ToString());

        var canonicalType = type.CanonicalType;

        if (canonicalType is ConstantArrayType { ElementType: RecordType } constantArrayType)
        {
            canonicalType = constantArrayType.ElementType;
        }

        if (canonicalType is RecordType recordType && name.StartsWith("__AnonymousRecord_"))
        {
            var recordDecl = recordType.Decl;
            if (recordDecl.Parent is RecordDecl parentRecordDecl)
            {
                var matchingField =
                    parentRecordDecl.Fields.FirstOrDefault(fieldDecl => fieldDecl.Type.CanonicalType == recordType);

                if (matchingField != null)
                {
                    name = "_";
                    name += GetRemappedCursorName(matchingField);
                }
                else if (parentRecordDecl.AnonymousRecords.Count > 1)
                {
                    var index = parentRecordDecl.AnonymousRecords.IndexOf(cursor) + 1;
                    name += index.ToString();
                }
            }

            name += $"_e__{(recordDecl.IsUnion ? "Union" : "Struct")}";
        }

        return GetType(name);
    }

    private TypeSyntax? GetElementType(TypeSyntax type, out int arrayLevel)
    {
        switch (type)
        {
            case PointerTypeSyntax pointerTypeSyntax:
                arrayLevel = 1;
                return pointerTypeSyntax.ElementType;
            case ArrayTypeSyntax arrayTypeSyntax:
                arrayLevel = arrayTypeSyntax.RankSpecifiers.Count;
                return arrayTypeSyntax.ElementType;
            default:
                arrayLevel = 0;
                return null;
        }
    }

    private bool IsStructToClass(string name) => Config.StructToClasses.Contains(name);

    internal (Expr, Expr) ExtractArraySubscriptExpr(ArraySubscriptExpr arraySubscriptExpr)
    {
        return arraySubscriptExpr.Base.Type.CanonicalType is EnumType ||
               arraySubscriptExpr.Base.Type.CanonicalType.IsIntegerType
            ? (arraySubscriptExpr.Idx, arraySubscriptExpr.Base)
            : (arraySubscriptExpr.Base, arraySubscriptExpr.Idx);
    }



    internal bool IsInFunction(string name)
    {
        return _context.Any(x => x is FunctionDecl functionDecl && functionDecl.Name == name);
    }

    internal bool GetPrevContext<T>([MaybeNullWhen(false)] out T t) where T : Cursor
    {
        t = _context.Last?.Previous?.Value as T;
        return t != null;
    }

    internal bool IsPrevBoolContext(Expr expr)
    {
        return GetPrevContext<IfStmt>(out var ifStmt) && ifStmt.Cond == expr ||
               GetPrevContext<WhileStmt>(out var whileStmt) && whileStmt.Cond == expr ||
               GetPrevContext<DoStmt>(out var doStmt) && doStmt.Cond == expr ||
               GetPrevContext<ForStmt>(out var forStmt) && forStmt.Cond == expr ||
               GetPrevContext<ConditionalOperator>(out var conditionalOperator) && conditionalOperator.Cond == expr ||
               GetPrevContext<CallExpr>(out var callExpr) && callExpr.Spelling == "assert";
    }

    private FieldDeclarationSyntax CreateByteArrayField(string name, byte[] data)
    {
        return SyntaxFactory.FieldDeclaration(
                SyntaxFactory.VariableDeclaration(
                        SyntaxFactory.PointerType(
                                SyntaxFactory.PredefinedType(
                                    SyntaxFactory.Token(SyntaxKind.ByteKeyword)))
                            )
                    .WithVariables(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier(name))
                                .WithInitializer(
                                    SyntaxFactory.EqualsValueClause(
                                        SyntaxFactory.InvocationExpression(
                                                SyntaxFactory.IdentifierName("GetArrayPointer"))
                                            .WithArgumentList(
                                                SyntaxFactory.ArgumentList(
                                                    SyntaxFactory.SingletonSeparatedList(
                                                        SyntaxFactory.Argument(
                                                            SyntaxFactory.ArrayCreationExpression(
                                                                    SyntaxFactory.ArrayType(
                                                                            SyntaxFactory.PredefinedType(
                                                                                SyntaxFactory.Token(SyntaxKind
                                                                                    .ByteKeyword)))
                                                                        .WithRankSpecifiers(
                                                                            SyntaxFactory.SingletonList(
                                                                                SyntaxFactory.ArrayRankSpecifier(
                                                                                    SyntaxFactory
                                                                                        .SingletonSeparatedList<
                                                                                            ExpressionSyntax>(
                                                                                            SyntaxFactory
                                                                                                .OmittedArraySizeExpression())))))
                                                                .WithInitializer(
                                                                    SyntaxFactory.InitializerExpression(
                                                                        SyntaxKind.ArrayInitializerExpression,
                                                                        SyntaxFactory.SeparatedList<ExpressionSyntax>(
                                                                            data.Select(x =>
                                                                                SyntaxFactory.LiteralExpression(
                                                                                    SyntaxKind.NumericLiteralExpression,
                                                                                    SyntaxFactory
                                                                                        .Literal(x)))))))))))))))
            .WithModifiers(
                SyntaxFactory.TokenList(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                    SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)));
    }

    private ExpressionSyntax GetInnerExpression(ExpressionSyntax expression)
    {
        return expression is ParenthesizedExpressionSyntax parenthesizedExpression
            ? GetInnerExpression(parenthesizedExpression.Expression)
            : expression;
    }

    internal ExpressionSyntax NegateLogicalExpression(ExpressionSyntax expression)
    {
        var innerExpression = GetInnerExpression(expression);

        switch (innerExpression)
        {
            case BinaryExpressionSyntax binaryExpression:
                switch (binaryExpression.Kind())
                {
                    // ! x == y -> x != y
                    case SyntaxKind.EqualsExpression:
                        return SyntaxFactory.BinaryExpression(SyntaxKind.NotEqualsExpression, binaryExpression.Left, binaryExpression.Right);
                    // ! x != y -> x == y
                    case SyntaxKind.NotEqualsExpression:
                        return SyntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression, binaryExpression.Left, binaryExpression.Right);
                    // ! x < y -> x >= y
                    case SyntaxKind.LessThanExpression:
                        return SyntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression, binaryExpression.Left, binaryExpression.Right);
                    // ! x <= y -> x > y
                    case SyntaxKind.LessThanOrEqualExpression:
                        return SyntaxFactory.BinaryExpression(SyntaxKind.GreaterThanExpression, binaryExpression.Left, binaryExpression.Right);
                    // ! x > y -> x <= y
                    case SyntaxKind.GreaterThanExpression:
                        return SyntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression, binaryExpression.Left, binaryExpression.Right);
                    // ! x >= y -> x < y
                    case SyntaxKind.GreaterThanOrEqualExpression:
                        return SyntaxFactory.BinaryExpression(SyntaxKind.LessThanExpression, binaryExpression.Left, binaryExpression.Right);
                    // ! x || y -> ! x && ! y
                    case SyntaxKind.LogicalOrExpression:
                        return SyntaxFactory.BinaryExpression(SyntaxKind.LogicalAndExpression, NegateLogicalExpression(binaryExpression.Left), NegateLogicalExpression(binaryExpression.Right));
                    // ! x && y -> ! x || ! y
                    case SyntaxKind.LogicalAndExpression:
                        return SyntaxFactory.BinaryExpression(SyntaxKind.LogicalOrExpression, NegateLogicalExpression(binaryExpression.Left), NegateLogicalExpression(binaryExpression.Right));
                }

                break;
            // ! !x -> x
            case PrefixUnaryExpressionSyntax prefixUnaryExpression when prefixUnaryExpression.Kind() == SyntaxKind.LogicalNotExpression:
                return prefixUnaryExpression.Operand;
        }

        // ! x -> !x
        return SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, SyntaxFactory.ParenthesizedExpression(expression));
    }

    internal ExpressionSyntax ArrayCreationToInitializer(ExpressionSyntax expression, int rank)
    {
        var ret = expression;

        if (rank <= 0)
        {
            return ret;
        }

        // remove array creation
        // new int[X] { ... } -> { ... }
        if (ret is ArrayCreationExpressionSyntax {Initializer: { }} arrayCreationExpression)
        {
            ret = arrayCreationExpression.Initializer;
            rank--;
            if (rank <= 0)
            {
                return ret;
            }
        }

        // remove array creation inside
        if (ret is InitializerExpressionSyntax initializerExpression)
        {
            // { new int[X} { ... }, ... } -> { { ... }, ... }
            return SyntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression,
                SyntaxFactory.SeparatedList(
                    initializerExpression.Expressions.Select(e => ArrayCreationToInitializer(e, rank))));
        }

        return ret;
    }

    internal bool UseFunctionPointerForType(string name) 
        => Config.UseFunctionPointers && !Config.ExcludeFunctionPointers.Contains(name);
}

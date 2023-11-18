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
                    FieldsEnumerator()));
    }

    private static T AddBodyToMethodDeclaration<T>(T method, BlockSyntax body) where T : BaseMethodDeclarationSyntax =>
        body.Statements.Count == 1 && body.Statements[0] is ReturnStatementSyntax returnStatement
            ? (T) method.WithExpressionBody(SyntaxFactory.ArrowExpressionClause(returnStatement.Expression!))
                .WithSemicolonToken(
                    SyntaxFactory.Token(SyntaxKind.SemicolonToken))
            : (T) method.WithBody(body);

    private static AccessorDeclarationSyntax AddBodyToAccessorDeclaration(AccessorDeclarationSyntax method,
        BlockSyntax body) =>
        body.Statements.Count == 1 && body.Statements[0] is ReturnStatementSyntax returnStatement
            ? method.WithExpressionBody(SyntaxFactory.ArrowExpressionClause(returnStatement.Expression!))
                .WithSemicolonToken(
                    SyntaxFactory.Token(SyntaxKind.SemicolonToken))
            : method.WithBody(body);

    private ExpressionStatementSyntax GetILSizeofStatement(TypeSyntax type) =>
        SyntaxFactory.ExpressionStatement(
            type is PointerTypeSyntax
                ? SyntaxFactory.InvocationExpression(
                        SyntaxFactory.IdentifierName("Sizeof"))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(
                                    SyntaxFactory.ObjectCreationExpression(
                                            SyntaxFactory.IdentifierName("TypeRef"))
                                        .WithArgumentList(
                                            SyntaxFactory.ArgumentList(
                                                SyntaxFactory.SingletonSeparatedList(
                                                    SyntaxFactory.Argument(
                                                        SyntaxFactory.TypeOfExpression(type)))))))))
                : SyntaxFactory.InvocationExpression(
                    SyntaxFactory.GenericName(
                            SyntaxFactory.Identifier("Sizeof"))
                        .WithTypeArgumentList(
                            SyntaxFactory.TypeArgumentList(
                                SyntaxFactory.SingletonSeparatedList(type)))));

    private ExpressionSyntax GetReturnPointerExpression(TypeSyntax type) =>
        type is PointerTypeSyntax
            ? SyntaxFactory.CastExpression(
                SyntaxFactory.PointerType(type),
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("IL"),
                        SyntaxFactory.IdentifierName("ReturnPointer"))))
            : SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("IL"),
                        SyntaxFactory.GenericName(
                                SyntaxFactory.Identifier("ReturnPointer"))
                            .WithTypeArgumentList(
                                SyntaxFactory.TypeArgumentList(
                                    SyntaxFactory.SingletonSeparatedList(type)))));

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

        if (UnwrapElaborated(pointeeType) is TypedefType typedefType &&
            typedefType.Decl.UnderlyingType is FunctionProtoType functionProtoType)
        {
            if (UseFunctionPointerForType(pointeeType.AsString))
            {
                var functionPointerType = GetFunctionPointerType(cursor, functionProtoType);
                return Config.HideFunctionPointers
                    ? HideFunctionPointer(functionPointerType)
                    : functionPointerType;
            }

            return GetType(pointeeType.AsString);
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
        type = UnwrapElaborated(type);

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
                            if (!Config.HideFunctionPointers && useFunctionPointer && arrayLevel > 1)
                            {
                                _reporter.Report(DiagnosticLevel.Warning, $"Nested array with function pointer {name}");
                            }

                            if (useFunctionPointer)
                            {
                                var functionPointerType = GetFunctionPointerType(cursor, functionProtoType);
                                cSharpType = Config.HideFunctionPointers
                                    ? HideFunctionPointer(functionPointerType)
                                    : functionPointerType;
                            }
                            else
                            {
                                cSharpType = GetType(name);
                            }
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

    internal FunctionProtoType? GetFunctionProtoType(Expr expr)
    {
        var exprType = expr.Type;
        if (exprType is PointerType pointerType)
        {
            var pointeeType = pointerType.PointeeType;
            if (pointeeType is FunctionProtoType functionProtoType)
            {
                return functionProtoType;
            }

            if (UnwrapElaborated(pointeeType) is TypedefType typedefType &&
                typedefType.Decl.UnderlyingType is FunctionProtoType functionProtoType2)
            {
                return functionProtoType2;
            }
        }

        return null;
    }

    internal Type UnwrapElaborated(Type type) => type is ElaboratedType elaboratedType ? UnwrapElaborated(elaboratedType.NamedType) : type;

    internal FunctionProtoType? GetCalleeFunctionProtoType(Expr callee)
    {
        if (UnwrapElaborated(callee.Type) is TypedefType typedefType2 && typedefType2.Decl.UnderlyingType is PointerType
            {
                PointeeType: FunctionProtoType functionProtoType
            } && UseFunctionPointerForType(typedefType2.AsString))
        {
            return functionProtoType;
        }

        if (callee.Type is PointerType type &&
            UnwrapElaborated(type.PointeeType) is TypedefType
            {
                Decl.UnderlyingType: FunctionProtoType functionProtoType2
            } typedefType && UseFunctionPointerForType(typedefType.AsString))
        {
            return functionProtoType2;
        }

        return null;
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

        return GetType(name).WithTriviaFrom(cSharpType);
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

    private static SyntaxKind GetAccessSpecifier(SyntaxTokenList modifiers)
        => modifiers
            .Select(m => m.Kind())
            .FirstOrDefault(m =>
                m is SyntaxKind.PrivateKeyword or SyntaxKind.PublicKeyword or SyntaxKind.ProtectedKeyword
                    or SyntaxKind.InternalKeyword);

    private FieldDeclarationSyntax CreateArrayOptimization(FieldDeclarationSyntax fieldDeclarationSyntax)
    {
        var accessSpecifier = GetAccessSpecifier(fieldDeclarationSyntax.Modifiers);

        if (fieldDeclarationSyntax.Declaration.Type is not PointerTypeSyntax pointerType ||
            fieldDeclarationSyntax.Declaration.Variables.Count != 1)
        {
            _reporter.Report(DiagnosticLevel.Error, "Invalid use of CreateArrayOptimization");
            return fieldDeclarationSyntax;
        }

        var initializer = fieldDeclarationSyntax.Declaration.Variables[0].Initializer?.Value;
        if (!(initializer is InvocationExpressionSyntax
              {
                  Expression: IdentifierNameSyntax identifierName
              } invocationExpression &&
              identifierName.ToString() == "GetArrayPointer" &&
              invocationExpression.ArgumentList.Arguments.Count == 1 &&
              invocationExpression.ArgumentList.Arguments[0].Expression is ArrayCreationExpressionSyntax
                  arrayPointerInitializer))
        {
            _reporter.Report(DiagnosticLevel.Error, "Invalid use of CreateArrayOptimization");
            return fieldDeclarationSyntax;
        }

        var identifier = fieldDeclarationSyntax.Declaration.Variables[0].Identifier;
        var directiveName = Net7SpanArrayCreation(pointerType.ElementType)
                ? "NET7_0_OR_GREATER"
                : "NET8_0_OR_GREATER";
        AddUsing("System");
        AddUsing("System.Runtime.InteropServices");

        var spanPropertyName = $"Span_{identifier}";
        var spanPropertyDeclarationSyntax = SyntaxFactory.PropertyDeclaration(
                SyntaxFactory.GenericName(
                        SyntaxFactory.Identifier("ReadOnlySpan"))
                    .WithTypeArgumentList(
                        SyntaxFactory.TypeArgumentList(
                            SyntaxFactory.SingletonSeparatedList(pointerType.ElementType))),
                SyntaxFactory.Identifier(spanPropertyName))
            .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
            .WithExpressionBody(SyntaxFactory.ArrowExpressionClause(arrayPointerInitializer))
            .WithSemicolonToken(
                SyntaxFactory.Token(SyntaxKind.SemicolonToken))
            .WithLeadingTrivia(SyntaxFactory.Trivia(
                SyntaxFactory.IfDirectiveTrivia(
                    SyntaxFactory.IdentifierName(directiveName),
                    true,
                    false,
                    false)));

        var propertyDeclarationSyntax = SyntaxFactory
            .PropertyDeclaration(pointerType, identifier)
            .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(accessSpecifier),
                SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
            .WithExpressionBody(SyntaxFactory.ArrowExpressionClause(
                SyntaxFactory.CastExpression(pointerType,
                    SyntaxFactory
                        .InvocationExpression(
                            SyntaxFactory.IdentifierName("System.Runtime.CompilerServices.Unsafe.AsPointer"))
                        .WithArgumentList(
                            SyntaxFactory.ArgumentList(
                                SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.Argument(
                                        SyntaxFactory.InvocationExpression(
                                                SyntaxFactory.IdentifierName(
                                                    "MemoryMarshal.GetReference"))
                                            .WithArgumentList(
                                                SyntaxFactory.ArgumentList(
                                                    SyntaxFactory.SingletonSeparatedList(
                                                        SyntaxFactory.Argument(
                                                            SyntaxFactory.IdentifierName(spanPropertyName)
                                                        ))))
                                    ).WithRefOrOutKeyword(SyntaxFactory.Token(SyntaxKind.RefKeyword))))))
            ))
            .WithSemicolonToken(
                SyntaxFactory.Token(SyntaxKind.SemicolonToken))
            .WithTrailingTrivia(SyntaxFactory.Trivia(SyntaxFactory.ElseDirectiveTrivia(true, false)));

        AddMethodsMember(spanPropertyDeclarationSyntax);
        AddMethodsMember(propertyDeclarationSyntax);

        return fieldDeclarationSyntax.WithTrailingTrivia(SyntaxFactory.Trivia(
            SyntaxFactory.EndIfDirectiveTrivia(
                false)));
    }

    private FieldDeclarationSyntax CreateByteArrayField(string name, byte[] data)
    {
        var fieldDeclarationSyntax = SyntaxFactory.FieldDeclaration(
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
                    SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                    SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)));
        if (Config.ArrayCreateOptimization)
        {
            fieldDeclarationSyntax = CreateArrayOptimization(fieldDeclarationSyntax);
        }
        return fieldDeclarationSyntax;
    }

    private ExpressionSyntax GetInnerExpression(ExpressionSyntax expression)
        => expression switch
        {
            ParenthesizedExpressionSyntax parenthesizedExpression => GetInnerExpression(parenthesizedExpression.Expression),
            _ => expression
        };

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

    internal static bool Net7SpanArrayCreation(TypeSyntax typeSyntax) => typeSyntax.ToString() is "byte" or "sbyte" or "bool";

    internal static bool Net8SpanArrayCreation(TypeSyntax typeSyntax) => Net7SpanArrayCreation(typeSyntax) ||
                                                                      typeSyntax.ToString() is "short" or "ushort"
                                                                          or "char" or "int"
                                                                          or "uint" or "long" or "ulong" or "double"
                                                                          or "float";

    internal bool IsArtificialFixedBufferAccess(Expr expr, [MaybeNullWhen(false)] out Expr subExpression)
    {
        if (expr is ImplicitCastExpr implicitCastExpr)
        {
            if (implicitCastExpr.CastKind == CX_CastKind.CX_CK_ArrayToPointerDecay &&
                GetExprAsWritten(implicitCastExpr.SubExpr, true) is MemberExpr memberExpr)
            {
                var type = memberExpr.Type;
                var cSharpType = GetRemappedCSharpType(memberExpr, type, out _);
                if (type.CanonicalType is ConstantArrayType)
                {
                    var elementTypeSyntax = GetElementType(cSharpType, out _)!;
                    if (!IsSupportedFixedSizedBufferType(elementTypeSyntax.ToString()))
                    {
                        subExpression = implicitCastExpr.SubExpr;
                        return true;
                    }
                }
            }
        }

        subExpression = default;
        return false;
    }

    internal bool IsSupportedFixedBufferType(Expr expr) =>
        UnwrapElaborated(expr.Type) is TypedefType typedefType &&
        typedefType.Decl.UnderlyingType is ConstantArrayType constantArrayType &&
        IsSupportedFixedSizedBufferType(GetCSharpType(expr, constantArrayType.ElementType, out _)
            .ToString());

    internal Expr GetInnerExpr(Expr expr)
        => expr switch
        {
            ParenExpr parenExpr => GetInnerExpr(parenExpr.SubExpr),
            ImplicitCastExpr implicitCastExpr when implicitCastExpr.CastKind == CX_CastKind.CX_CK_NoOp => GetInnerExpr(implicitCastExpr.SubExpr),
            _ => expr
        };

    internal IEnumerable<Cursor> ReverseContext()
    {
        var previousContext = _context.Last;
        while (previousContext != null)
        {
            yield return previousContext.Value;
            previousContext = previousContext.Previous;
        }
    }

    internal IEnumerable<Cursor> GetContextBefore(Cursor cursor) => 
        ReverseContext().SkipWhile(c => c != cursor).Skip(1);

    private bool? IsVarDeclField(VarDecl varDecl)
    {
        var prevDecl = GetContextBefore(varDecl).FirstOrDefault(c => c is Decl);
        if (prevDecl is TranslationUnitDecl or LinkageSpecDecl or RecordDecl)
        {
            if (!varDecl.HasInit)
            {
                return null;
            }

            return true;
        }

        var type = varDecl.Type;
        if (varDecl.Kind != CX_DeclKind.CX_DeclKind_ParmVar && type is ArrayType &&
            (varDecl.StorageClass == CX_StorageClass.CX_SC_Static || type.CanonicalType.IsLocalConstQualified))
        {
            return true;
        }
        return false;
    }

    private void AddInitConstructor(string typeName, TypeDeclarationSyntax typeDeclaration, RecordDecl decl, FileBuilder fileBuilder)
    {
        if (!_projectBuilder.AddInitConstructor(fileBuilder.Name))
        {
            return;
        }

        var constructorDeclarationSyntax = SyntaxFactory.ConstructorDeclaration(SyntaxFactory.Identifier(typeName))
            .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
            .WithParameterList(SyntaxFactory.ParameterList(SyntaxFactory.SeparatedList(GetConstructorArgs())))
            .WithBody(SyntaxFactory.Block(GetConstructorStatements()));

        var newTypeDeclarationSyntax = typeDeclaration.AddMembers(constructorDeclarationSyntax);
        fileBuilder.ReplaceMember(typeDeclaration, newTypeDeclarationSyntax);

        IEnumerable<ParameterSyntax> GetConstructorArgs()
        {
            foreach (var fieldDecl in decl.Fields)
            {
                var fieldName = GetRemappedCursorName(fieldDecl);
                var type = fieldDecl.Type;
                var cSharpType = GetRemappedCSharpType(fieldDecl, type, out _);
                yield return SyntaxFactory.Parameter(
                        SyntaxFactory.Identifier(fieldName))
                    .WithType(cSharpType)
                    .WithDefault(
                        SyntaxFactory.EqualsValueClause(
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.DefaultLiteralExpression,
                                SyntaxFactory.Token(SyntaxKind.DefaultKeyword))));
            }
        }

        IEnumerable<StatementSyntax> GetConstructorStatements()
        {
            foreach (var fieldDecl in decl.Fields)
            {
                var fieldName = GetRemappedCursorName(fieldDecl);
                yield return SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.ThisExpression(),
                            SyntaxFactory.IdentifierName(fieldName)),
                        SyntaxFactory.IdentifierName(fieldName)));
            }
        }
    }

    private TypeSyntax HideFunctionPointer(TypeSyntax typeSyntax) 
        => GetType("void*");
}

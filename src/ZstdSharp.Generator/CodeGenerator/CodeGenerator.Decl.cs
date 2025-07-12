using System;
using System.Collections.Generic;
using System.Linq;
using ClangSharp;
using ClangSharp.Interop;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Type = ClangSharp.Type;

namespace ZstdSharp.Generator.CodeGenerator;

internal partial class CodeGenerator
{
    private SyntaxNode? VisitDecl(Decl decl)
    {
        if (IsExcluded(decl))
        {
            return null;
        }

        switch (decl)
        {
            case TranslationUnitDecl translationUnitDecl:
                return VisitTranslationUnit(translationUnitDecl);
            case FunctionDecl functionDecl:
                return VisitFunctionDecl(functionDecl);
            case VarDecl varDecl:
                return VisitVarDecl(varDecl);
            case FieldDecl fieldDecl:
                return VisitFieldDecl(fieldDecl);
            case TypedefDecl typedefDecl:
                return VisitTypedefDecl(typedefDecl);
            case RecordDecl recordDecl:
                return VisitRecordDecl(recordDecl);
            case StaticAssertDecl staticAssertDecl:
                return VisitStaticAssertDecl(staticAssertDecl);
            case EnumDecl enumDecl:
                return VisitEnumDecl(enumDecl);
            case EnumConstantDecl enumConstantDecl:
                return VisitEnumConstantDecl(enumConstantDecl);
            default:
                Report(DiagnosticLevel.Error, $"Type {decl.GetType()} is not implemented yet");
                return null;
        }
    }

    private SyntaxNode? VisitTranslationUnit(TranslationUnitDecl translationUnitDecl)
    {
        foreach (var cursor in translationUnitDecl.CursorChildren)
        {
            var syntaxNode = Visit(cursor);
            if (syntaxNode != null)
            {
                Report(DiagnosticLevel.Error, "Syntax node must be null on top level");
            }
        }

        if (_statementConsumers.Count != 0)
        {
            Report(DiagnosticLevel.Error, "Statement consumers is not empty");
        }

        if (_builders.Count != 0)
        {
            Report(DiagnosticLevel.Error, "Builders is not empty");
        }
        return null;
    }

    private SyntaxNode? VisitFunctionDecl(FunctionDecl functionDecl)
    {
        if (IsExcluded(functionDecl))
        {
            return null;
        }

        if (!functionDecl.HasBody || !functionDecl.IsThisDeclarationADefinition)
        {
            return null;
        }

        var nativeName = GetCursorName(functionDecl);
        var accessSpecifier = GetAccessSpecifierKind(functionDecl);
        var name = GetRemappedName(nativeName);
        var escapedName = EscapeName(name);

        if (!_projectBuilder.AddGeneratedType(name))
        {
            return null;
        }

        StartFile(GetMethodsFileName(functionDecl));

        var returnType = functionDecl.ReturnType;
        var returnCSharpType = GetRemappedCSharpType(functionDecl, returnType, out _);

        var callReplacement = _callReplacer.GetCallReplacement(name);
        if (callReplacement != null)
        {
            returnCSharpType = GetType(callReplacement.Type.Name);
        }

        var isInlined = functionDecl.IsInlined || (
            functionDecl.HasAttrs &&
            functionDecl.Attrs.Any(x => x.Kind == CX_AttrKind.CX_AttrKind_AlwaysInline)
        ) || Config.JitInlineMethods.Contains(name);

        var methodDeclaration = SyntaxFactory.MethodDeclaration(
                returnCSharpType,
                SyntaxFactory.Identifier(escapedName)
            )
            .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(accessSpecifier),
                SyntaxFactory.Token(SyntaxKind.StaticKeyword)));

        foreach (var p in functionDecl.Parameters)
        {
            var localDeclarationStatementSyntax = Visit<LocalDeclarationStatementSyntax>(p);
            if (localDeclarationStatementSyntax != null)
            {
                var variableDeclaratorSyntax = localDeclarationStatementSyntax.Declaration.Variables.FirstOrDefault();
                if (variableDeclaratorSyntax != null)
                {
                    methodDeclaration =
                        methodDeclaration.AddParameterListParameters(
                            SyntaxFactory.Parameter(variableDeclaratorSyntax.Identifier)
                                .WithType(localDeclarationStatementSyntax.Declaration.Type)
                                .WithDefault(variableDeclaratorSyntax.Initializer));
                }
                else
                {
                    Report(DiagnosticLevel.Error, "Unknown parameter");
                }
            }
            else
            {
                Report(DiagnosticLevel.Error, "Unknown parameter");
            }
        }

        if (isInlined)
        {
            AddUsing("System.Runtime.CompilerServices");

            methodDeclaration = methodDeclaration.AddAttributeLists(
                SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("MethodImpl"),
                            SyntaxFactory.ParseAttributeArgumentList("(MethodImplOptions.AggressiveInlining)")
                        )
                    )
                )
            );
        }

        if (Config.InlineMethods.Contains(name))
        {
            methodDeclaration = methodDeclaration.AddAttributeLists(
                SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("InlineMethod.Inline")))));
        }

        if (functionDecl.HasBody)
        {
            methodDeclaration = methodDeclaration.WithBody(Visit<BlockSyntax>(functionDecl.Body!));
        }

        methodDeclaration = WithComments(methodDeclaration, functionDecl);

        AddMethodsMember(methodDeclaration);

        StopFile();

        return null;
    }

    private SyntaxNode? VisitVarDecl(VarDecl varDecl)
    {
        var nativeName = GetCursorName(varDecl);
        var accessSpecifier = GetAccessSpecifierKind(varDecl);
        var name = GetRemappedName(nativeName);
        var escapedName = EscapeName(name);

        var type = varDecl.Type;
        var cSharpType = GetRemappedCSharpType(varDecl, type, out var arrayConvertedToPointer);

        var isVarDeclField = IsVarDeclField(varDecl);
        // skip
        if (isVarDeclField == null)
        {
            return null;
        }

        var isField = isVarDeclField.Value;
        if (isField)
        {
            var fileName = GetMethodsFileName(varDecl);

            if (!_projectBuilder.AddGeneratedType(name))
            {
                return null;
            }

            StartFile(fileName!);
        }

        var isLocal = !isField && varDecl is not ParmVarDecl;

        var initializer = varDecl.HasInit ? Visit<ExpressionSyntax>(varDecl.Init) : null;
        var additionalStatements = new List<StatementSyntax>();

        // XXX name = ""; -> string name = "";
        if (initializer is LiteralExpressionSyntax literalExpressionSyntax2 &&
            literalExpressionSyntax2.Kind() == SyntaxKind.StringLiteralExpression)
        {
            cSharpType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.StringKeyword));
        }

        // constant array initializer
        if (initializer == null && isLocal && type is ConstantArrayType constantArrayType)
        {
            var elementTypeSyntax = GetElementType(cSharpType, out var arrayLevel);

            if (elementTypeSyntax != null)
            {
                // typeName[Size][][][]...
                initializer = SyntaxFactory.ArrayCreationExpression(
                    SyntaxFactory.ArrayType(
                            elementTypeSyntax
                        )
                        .WithRankSpecifiers(new SyntaxList<ArrayRankSpecifierSyntax>(
                                SyntaxFactory.ArrayRankSpecifier(
                                    SyntaxFactory.SeparatedList(
                                        Enumerable.Repeat<ExpressionSyntax>(
                                                SyntaxFactory.LiteralExpression(
                                                    SyntaxKind.NumericLiteralExpression,
                                                    SyntaxFactory.Literal((int) constantArrayType.Size)
                                                ), 1
                                            )
                                            .Concat(
                                                Enumerable.Repeat(SyntaxFactory.OmittedArraySizeExpression(),
                                                    arrayLevel - 1)
                                            )
                                    )
                                )
                            )
                        )
                );
            }
        }

        ExpressionSyntax? arrayPointerInitializer = null;

        // initializer to stackalloc / GetArrayPointer
        if (initializer != null && arrayConvertedToPointer)
        {
            AddUsing("static ZstdSharp.UnsafeHelper");

            // tail with defaults? -> memset
            if (!isField && initializer is ArrayCreationExpressionSyntax {Initializer: { }} arrayCreationExpressionSyntax)
            {
                var size = arrayCreationExpressionSyntax.Initializer.Expressions.Count;
                var index = size - 1;

                while (index >= 0 &&
                       arrayCreationExpressionSyntax.Initializer.Expressions[index] is LiteralExpressionSyntax
                           literalExpressionSyntax &&
                       literalExpressionSyntax.Kind() == SyntaxKind.DefaultLiteralExpression)
                {
                    index--;
                }

                index++;

                if (index < size)
                {
                    for (var i = 0; i < index; i++)
                    {
                        additionalStatements.Add(SyntaxFactory.ExpressionStatement(SyntaxFactory.AssignmentExpression(
                            SyntaxKind.SimpleAssignmentExpression,
                            SyntaxFactory.ElementAccessExpression(
                                    SyntaxFactory.IdentifierName(escapedName))
                                .WithArgumentList(
                                    SyntaxFactory.BracketedArgumentList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.Argument(
                                                SyntaxFactory.LiteralExpression(
                                                    SyntaxKind.NumericLiteralExpression,
                                                    SyntaxFactory.Literal(i)
                                                ))))),
                            arrayCreationExpressionSyntax.Initializer.Expressions[i])));
                    }

                    initializer = arrayCreationExpressionSyntax.WithInitializer(null);
                    // add zero init
                    additionalStatements.Add(SyntaxFactory.ExpressionStatement(CreateZeroInit(escapedName, GetElementType(cSharpType, out _)!, index, size)));
                }
            }

            if (!isField && initializer is ArrayCreationExpressionSyntax arrayCreationExpressionSyntax2)
            {
                initializer = SyntaxFactory.StackAllocArrayCreationExpression(arrayCreationExpressionSyntax2.Type).WithInitializer(arrayCreationExpressionSyntax2.Initializer);
            }
            else
            {
                arrayPointerInitializer = initializer;

                initializer = SyntaxFactory.InvocationExpression(
                        SyntaxFactory.IdentifierName("GetArrayPointer"))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(initializer)
                            )
                        )
                    );
            }
        }

        // int[,] x = new int[X][] { new int[Y] { ... }, ... } -> int[,] x = { { ... }, ... }
        if (Config.ConvertNestedArraysToMultidimensional &&
            initializer is ArrayCreationExpressionSyntax &&
            cSharpType is ArrayTypeSyntax {RankSpecifiers.Count: 1} arrayTypeSyntax &&
            arrayTypeSyntax.RankSpecifiers[0].Rank > 1)
        {
            initializer = ArrayCreationToInitializer(initializer, arrayTypeSyntax.RankSpecifiers[0].Rank);
        }

        var variableDeclarationSyntax = SyntaxFactory.VariableDeclaration(
            cSharpType,
            SyntaxFactory.SingletonSeparatedList(
                SyntaxFactory.VariableDeclarator(
                    SyntaxFactory.Identifier(escapedName),
                    default,
                    initializer != null ? SyntaxFactory.EqualsValueClause(initializer) : null
                )
            )
        );

        if (!isField)
        {
            var localDeclarationStatementSyntax = SyntaxFactory.LocalDeclarationStatement(variableDeclarationSyntax);

            if (cSharpType is not PointerTypeSyntax && type.CanonicalType.IsLocalConstQualified && initializer != null && IsConstExpr(initializer))
            {
                localDeclarationStatementSyntax =
                    localDeclarationStatementSyntax.WithModifiers(
                        SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.ConstKeyword)));
            }

            if (additionalStatements.Count > 0)
            {
                AddStatementToConsumer(localDeclarationStatementSyntax);

                foreach (var additionalStatement in additionalStatements.Take(additionalStatements.Count - 1))
                {
                    AddStatementToConsumer(additionalStatement);
                }

                return additionalStatements.Last();
            }

            return localDeclarationStatementSyntax;
        }

        var fieldDeclarationSyntax = SyntaxFactory.FieldDeclaration(variableDeclarationSyntax)
            .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(accessSpecifier)));

        if (type.CanonicalType.IsLocalConstQualified || arrayConvertedToPointer)
        {
            if (initializer != null && IsConstExpr(initializer))
            {
                // nuint -> uint
                fieldDeclarationSyntax = ConvertConstFieldType(fieldDeclarationSyntax, "nuint", "uint", uint.MinValue,
                    uint.MaxValue);
                // nint -> int
                fieldDeclarationSyntax = ConvertConstFieldType(fieldDeclarationSyntax, "nint", "int", int.MinValue,
                    int.MaxValue);
                fieldDeclarationSyntax =
                    fieldDeclarationSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.ConstKeyword));

                FieldDeclarationSyntax ConvertConstFieldType(FieldDeclarationSyntax field, string sourceType,
                    string destType, long minValue, long maxValue)
                {
                    if (field.Declaration.Type.ToString() == sourceType)
                    {
                        var evalResult = varDecl.Init.Handle.Evaluate;
                        if (evalResult.Kind == CXEvalResultKind.CXEval_Int && evalResult.AsLongLong >= minValue &&
                            evalResult.AsLongLong <= maxValue)
                        {
                            field =
                                field.WithDeclaration(
                                    field.Declaration.WithType(GetType(destType)));
                        }
                        else
                        {
                            _reporter.Report(DiagnosticLevel.Warning,
                                $"Const {sourceType} are not supported by IL2CPP (Unity), value is out bounds");
                        }
                    }

                    return field;
                }
            }
            else
            {
                fieldDeclarationSyntax =
                    fieldDeclarationSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                        SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword));
            }
        }
        else
        {
            fieldDeclarationSyntax = fieldDeclarationSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.StaticKeyword));
        }

        if (Config.ArrayCreateOptimization && arrayPointerInitializer != null &&
            fieldDeclarationSyntax.Declaration.Type is PointerTypeSyntax pointerType &&
            Net7SpanArrayCreation(pointerType.ElementType))
        {
            fieldDeclarationSyntax = CreateArrayOptimization(fieldDeclarationSyntax);
        }

        AddMethodsMember(fieldDeclarationSyntax);

        StopFile();

        return null;
    }

    ExpressionSyntax CreateZeroInit(string escapedName, TypeSyntax type, int index, int size) =>
        SyntaxFactory.InvocationExpression(
                SyntaxFactory.IdentifierName("memset"))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList(
                        new List<ArgumentSyntax>
                        {
                            // name
                            SyntaxFactory.Argument(
                                index > 0
                                    ? SyntaxFactory.BinaryExpression(
                                        SyntaxKind.AddExpression,
                                        SyntaxFactory.IdentifierName(escapedName),
                                        SyntaxFactory.LiteralExpression(
                                            SyntaxKind.NumericLiteralExpression,
                                            SyntaxFactory.Literal(index)))
                                    : SyntaxFactory.IdentifierName(escapedName)
                            ),
                            SyntaxFactory.Argument(
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    SyntaxFactory.Literal(0))),
                            SyntaxFactory.Argument(
                                SyntaxFactory.BinaryExpression(
                                    SyntaxKind.MultiplyExpression,
                                    SyntaxFactory.SizeOfExpression(
                                        type
                                    ),
                                    SyntaxFactory.LiteralExpression(
                                        SyntaxKind.NumericLiteralExpression,
                                        SyntaxFactory.Literal(size - index)
                                    )))
                        })));

    private SyntaxNode VisitEnumConstantDecl(EnumConstantDecl enumConstantDecl)
    {
        var name = GetRemappedCursorName(enumConstantDecl);
        var escapedName = EscapeName(name);
        var enumMemberDeclarationSyntax = SyntaxFactory.EnumMemberDeclaration(SyntaxFactory.Identifier(escapedName));
        if (enumConstantDecl.InitExpr != null)
        {
            enumMemberDeclarationSyntax =
                enumMemberDeclarationSyntax.WithEqualsValue(
                    SyntaxFactory.EqualsValueClause(Visit<ExpressionSyntax>(enumConstantDecl.InitExpr)!));
        }

        return enumMemberDeclarationSyntax;
    }

    private SyntaxNode? VisitEnumDecl(EnumDecl enumDecl)
    {
        var accessSpecifier = GetAccessSpecifierKind(enumDecl);
        var name = GetTypeFileName(enumDecl);
        var escapedName = EscapeName(name);

        if (!_projectBuilder.AddGeneratedType(name))
        {
            return null;
        }

        StartFile(name);

        var cSharpType = GetRemappedCSharpType(enumDecl, enumDecl.IntegerType, out _);

        var enumDeclarationSyntax =
            SyntaxFactory.EnumDeclaration(SyntaxFactory.Identifier(escapedName))
                .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(accessSpecifier)));

        // todo
        if (!cSharpType.ToString().Equals("int"))
        {
            enumDeclarationSyntax = enumDeclarationSyntax.AddBaseListTypes(SyntaxFactory.SimpleBaseType(cSharpType));
        }

        enumDeclarationSyntax = enumDeclarationSyntax.AddMembers(enumDecl.Enumerators.Select(Visit<EnumMemberDeclarationSyntax>).ToArray()!);
        enumDeclarationSyntax = enumDeclarationSyntax.AddMembers(enumDecl.Decls.Except(enumDecl.Enumerators).Select(Visit<EnumMemberDeclarationSyntax>).ToArray()!);

        // comment
        enumDeclarationSyntax = WithComments(enumDeclarationSyntax, enumDecl);

        AddMember(enumDeclarationSyntax);

        StopFile();

        return null;
    }

    private SyntaxNode? VisitStaticAssertDecl(StaticAssertDecl staticAssertDecl)
    {
        Report(DiagnosticLevel.Error, $"Type {staticAssertDecl.GetType()} is not implemented yet");
        return null;
    }

    private SyntaxNode? VisitRecordDecl(RecordDecl recordDecl)
    {
        var name = GetTypeFileName(recordDecl);
        var escapedName = EscapeName(name);

        if (!recordDecl.IsThisDeclarationADefinition)
        {
            return null;
        }

        if (!_projectBuilder.AddGeneratedType(name))
        {
            return null;
        }

        StartFile(name);

        var cxxRecordDecl = recordDecl as CXXRecordDecl;

        var alignment = recordDecl.TypeForDecl.Handle.AlignOf;
        var maxAlignment = recordDecl.Fields.Any()
            ? recordDecl.Fields.Max(fieldDecl => fieldDecl.Type.Handle.AlignOf)
            : alignment;

        TypeDeclarationSyntax structDeclarationSyntax = IsStructToClass(name)
            ? SyntaxFactory.ClassDeclaration(escapedName)
            : SyntaxFactory.StructDeclaration(escapedName);

        structDeclarationSyntax = structDeclarationSyntax
            .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(GetAccessSpecifierKind(recordDecl))));

        var members = new List<MemberDeclarationSyntax>();
        RegisterMemberConsumer(memberDeclarationSyntax => members.Add(memberDeclarationSyntax));

        // unsafe
        if (IsUnsafe(recordDecl))
        {
            structDeclarationSyntax =
                structDeclarationSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.UnsafeKeyword));
        }

        if (recordDecl.IsUnion)
        {
            AddUsing("System.Runtime.InteropServices");
            var list = new SeparatedSyntaxList<AttributeArgumentSyntax>()
                .Add(SyntaxFactory.AttributeArgument(
                    SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("LayoutKind"),
                        SyntaxFactory.IdentifierName("Explicit")))
                );

            if (alignment < maxAlignment)
            {
                list = list.Add(SyntaxFactory.AttributeArgument(
                    SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression, SyntaxFactory.IdentifierName("Pack"),
                        SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                            SyntaxFactory.Literal(alignment))
                        )
                    )
                );
            }

            var singletonSeparatedList = SyntaxFactory.SingletonSeparatedList(
                SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("StructLayout"),
                    SyntaxFactory.AttributeArgumentList(
                        list
                    )
                )
            );
            structDeclarationSyntax = structDeclarationSyntax.AddAttributeLists(
                SyntaxFactory.AttributeList(
                    singletonSeparatedList
                )
            );

        }
        else if (alignment < maxAlignment)
        {
            AddUsing("System.Runtime.InteropServices");
            structDeclarationSyntax = structDeclarationSyntax.AddAttributeLists(
                SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("StructLayout"),
                            SyntaxFactory.AttributeArgumentList(
                                SyntaxFactory.SeparatedList(
                                    new[]
                                    {
                                        SyntaxFactory.AttributeArgument(
                                            SyntaxFactory.MemberAccessExpression(
                                                SyntaxKind.SimpleMemberAccessExpression,
                                                SyntaxFactory.IdentifierName("LayoutKind"),
                                                SyntaxFactory.IdentifierName("Sequential")
                                            )
                                        ),
                                        SyntaxFactory.AttributeArgument(
                                            SyntaxFactory.AssignmentExpression(
                                                SyntaxKind.SimpleAssignmentExpression,
                                                SyntaxFactory.IdentifierName("Pack"),
                                                SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                                    SyntaxFactory.Literal(alignment)
                                                )
                                            )
                                        )
                                    }
                                )
                            )
                        ))
                )
            );
        }

        foreach (var declaration in recordDecl.Decls)
        {
            switch (declaration)
            {
                case FieldDecl fieldDecl:
                {
                    if (!IsConstSize(fieldDecl) && !Config.VariableSizeTypes.ContainsKey(name))
                    {
                        Report(DiagnosticLevel.Warning, $"Unhandled variable field declaration {fieldDecl.Name} in struct {name}");
                    }

                    if (fieldDecl.IsBitField)
                    {
                        Report(DiagnosticLevel.Info, $"Bit fields for structs is not supported {name}.{fieldDecl.Name}");
                    }

                    var fieldDeclarationSyntax = Visit<FieldDeclarationSyntax>(fieldDecl);
                    if (fieldDeclarationSyntax != null)
                    {
                        structDeclarationSyntax = structDeclarationSyntax.AddMembers(fieldDeclarationSyntax);
                    }

                    break;
                }
            }
        }

        foreach (var decl in recordDecl.Decls.Except(recordDecl.Fields.AsEnumerable()))
        {
            Visit(decl);
        }

        if (cxxRecordDecl != null && cxxRecordDecl.Bases.Any())
        {
            Report(DiagnosticLevel.Error, $"Bases for structs is not supported {name}");
        }

        if (cxxRecordDecl != null && cxxRecordDecl.Ctors.Any())
        {
            Report(DiagnosticLevel.Error, $"Ctors for structs is not supported {name}");
        }

        if (cxxRecordDecl != null && cxxRecordDecl.HasUserDeclaredDestructor)
        {
            Report(DiagnosticLevel.Error, $"HasUserDeclaredDestructor for structs is not supported {name}");
        }

        if (cxxRecordDecl != null && cxxRecordDecl.Methods.Any())
        {
            Report(DiagnosticLevel.Error, $"Methods for structs is not supported {name}");
        }

        if (members.Count > 0)
        {
            structDeclarationSyntax = structDeclarationSyntax.AddMembers(members.ToArray());
        }

        // comment
        structDeclarationSyntax = WithComments(structDeclarationSyntax, recordDecl);
        UnRegisterMemberConsumer();

        AddMember(structDeclarationSyntax);

        StopFile();

        return null;
    }

    private SyntaxNode? VisitTypedefDecl(TypedefDecl typedefDecl)
    {
        ForUnderlyingType(typedefDecl, typedefDecl.UnderlyingType);

        void ForFunctionProtoType(TypedefDecl typedefDecl, FunctionProtoType functionProtoType, Type? parentType)
        {
            var name = GetTypeFileName(typedefDecl);
            var escapedName = EscapeName(name);

            if (UseFunctionPointerForType(name))
            {
                return;
            }

            if (!_projectBuilder.AddGeneratedType(name))
            {
                return;
            }

            StartFile(name);

            AddUsing("System.Runtime.InteropServices");

            var callingConventionName = GetCallingConventionName(parentType is AttributedType
                    ? parentType.Handle.FunctionTypeCallingConv
                    : functionProtoType.CallConv);

            var returnType = functionProtoType.ReturnType;
            var returnCSharpType =
                GetRemappedCSharpType(typedefDecl, returnType, out _);

            var delegateDeclarationSyntax = SyntaxFactory
                .DelegateDeclaration(returnCSharpType, SyntaxFactory.Identifier(escapedName))
                .WithAttributeLists(
                    SyntaxFactory.SingletonList(
                        SyntaxFactory.AttributeList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Attribute(
                                        SyntaxFactory.IdentifierName("UnmanagedFunctionPointer"))
                                    .WithArgumentList(
                                        SyntaxFactory.AttributeArgumentList(
                                            SyntaxFactory.SingletonSeparatedList(
                                                SyntaxFactory.AttributeArgument(
                                                    SyntaxFactory.MemberAccessExpression(
                                                        SyntaxKind.SimpleMemberAccessExpression,
                                                        SyntaxFactory.IdentifierName("CallingConvention"),
                                                        SyntaxFactory.IdentifierName(callingConventionName))))))))))
                .WithModifiers(
                    SyntaxFactory.TokenList(SyntaxFactory.Token(GetAccessSpecifierKind(typedefDecl))));

            if (IsUnsafe(typedefDecl, functionProtoType))
            {
                delegateDeclarationSyntax = delegateDeclarationSyntax.AddModifiers(SyntaxFactory.Token(SyntaxKind.UnsafeKeyword));
            }

            int index = 0;
            foreach (var p in typedefDecl.CursorChildren.OfType<ParmVarDecl>())
            {
                var localDeclarationStatementSyntax = Visit<LocalDeclarationStatementSyntax>(p);
                if (localDeclarationStatementSyntax != null)
                {
                    var variableDeclaratorSyntax = localDeclarationStatementSyntax.Declaration.Variables.FirstOrDefault();
                    if (variableDeclaratorSyntax != null)
                    {
                        var identifier = variableDeclaratorSyntax.Identifier;
                        if (identifier.ToString() == "param")
                        {
                            identifier = SyntaxFactory.Identifier(identifier.ToString() + index);
                        }

                        delegateDeclarationSyntax =
                            delegateDeclarationSyntax.AddParameterListParameters(
                                SyntaxFactory.Parameter(identifier)
                                    .WithType(localDeclarationStatementSyntax.Declaration.Type)
                                    .WithDefault(variableDeclaratorSyntax.Initializer));
                    }
                    else
                    {
                        Report(DiagnosticLevel.Error, "Unknown parameter");
                    }
                }
                else
                {
                    Report(DiagnosticLevel.Error, "Unknown parameter");
                }

                index++;
            }

            AddMember(delegateDeclarationSyntax);

            StopFile();
        }

        void ForPointeeType(TypedefDecl innerTypedefDecl, Type? parentType, Type pointeeType)
        {
            pointeeType = UnwrapElaborated(pointeeType);

            if (pointeeType is AttributedType attributedType)
            {
                ForPointeeType(innerTypedefDecl, attributedType, attributedType.ModifiedType);
            }
            else if (pointeeType is FunctionProtoType functionProtoType)
            {
                ForFunctionProtoType(innerTypedefDecl, functionProtoType, parentType);
            }
            else if (pointeeType is PointerType pointerType)
            {
                ForPointeeType(innerTypedefDecl, pointerType, pointerType.PointeeType);
            }
            else if (pointeeType is TypedefType typedefType)
            {
                ForPointeeType(innerTypedefDecl, typedefType, typedefType.Decl.UnderlyingType);
            }
            else if (!(pointeeType is ConstantArrayType) && !(pointeeType is BuiltinType) && !(pointeeType is TagType))
            {
                Report(DiagnosticLevel.Error, $"Unsupported pointee type: {pointeeType.TypeClass}");
            }
        }

        void ForUnderlyingType(TypedefDecl innerTypedefDecl, Type underlyingType)
        {
            underlyingType = UnwrapElaborated(underlyingType);

            if (underlyingType is ArrayType)
            {
                // Nothing to do for array types
            }
            else if (underlyingType is AttributedType attributedType)
            {
                ForUnderlyingType(innerTypedefDecl, attributedType.ModifiedType);
            }
            else if (underlyingType is BuiltinType)
            {
                // Nothing to do for builtin types
            }
            else if (underlyingType is FunctionProtoType functionProtoType)
            {
                ForFunctionProtoType(innerTypedefDecl, functionProtoType, parentType: null);
            }
            else if (underlyingType is PointerType pointerType)
            {
                ForPointeeType(innerTypedefDecl, parentType: null, pointerType.PointeeType);
            }
            else if (underlyingType is ReferenceType referenceType)
            {
                ForPointeeType(innerTypedefDecl, parentType: null, referenceType.PointeeType);
            }
            else if (underlyingType is TagType)
            {
                // Nothing to do for tag types
            }
            else if (underlyingType is TypedefType typedefType)
            {
                ForUnderlyingType(innerTypedefDecl, typedefType.Decl.UnderlyingType);
            }
            else
            {
                Report(DiagnosticLevel.Error, $"Unsupported underlying type: {underlyingType.TypeClass}");
            }
        }

        return null;
    }

    private SyntaxNode? VisitFieldDecl(FieldDecl fieldDecl)
    {
        var accessSpecifier = GetAccessSpecifierKind(fieldDecl);
        var name = GetRemappedCursorName(fieldDecl);
        var escapedName = EscapeName(name);

        var type = fieldDecl.Type;
        var cSharpType = GetRemappedCSharpType(fieldDecl, type, out _);
        var variableDeclarationSyntax = SyntaxFactory.VariableDeclaration(
            cSharpType,
            SyntaxFactory.SingletonSeparatedList(
                SyntaxFactory.VariableDeclarator(
                    SyntaxFactory.Identifier(escapedName)
                )
            )
        );

        var fieldDeclarationSyntax = SyntaxFactory.FieldDeclaration(variableDeclarationSyntax)
            .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(accessSpecifier)));

        if (type.CanonicalType is ConstantArrayType constantArrayType)
        {
            var elementTypeSyntax = GetElementType(cSharpType, out _)!;

            // todo
            if (IsSupportedFixedSizedBufferType(elementTypeSyntax.ToString()))
            {
                ExpressionSyntax size = SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    SyntaxFactory.Literal(Math.Max((int) constantArrayType.Size, 1)));

                var elementType = constantArrayType.ElementType;

                while (elementType.CanonicalType is ConstantArrayType subConstantArrayType)
                {
                    size = SyntaxFactory.BinaryExpression(SyntaxKind.MultiplyExpression, size,
                        SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                            SyntaxFactory.Literal((int) Math.Max(subConstantArrayType.Size, 1))));

                    elementType = subConstantArrayType.ElementType;
                }

                fieldDeclarationSyntax = SyntaxFactory.FieldDeclaration(SyntaxFactory.VariableDeclaration(
                            elementTypeSyntax,
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier(escapedName)
                                ).WithArgumentList(
                                    SyntaxFactory.BracketedArgumentList(
                                        SyntaxFactory.SingletonSeparatedList(SyntaxFactory.Argument(size)
                                        )
                                    )
                                )
                            )
                        )
                    )
                    .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(accessSpecifier),
                        SyntaxFactory.Token(SyntaxKind.FixedKeyword)));
            }
            else
            {
                var fixedBufferName = GetArtificialFixedSizedBufferName(name);

                fieldDeclarationSyntax = SyntaxFactory.FieldDeclaration(SyntaxFactory.VariableDeclaration(
                            GetType(fixedBufferName),
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier(escapedName)
                                )
                            )
                        )
                    )
                    .WithModifiers(new SyntaxTokenList(SyntaxFactory.Token(accessSpecifier)));

                foreach (var member in CreateFixedBuffer(name, elementTypeSyntax, (int) constantArrayType.Size))
                {
                    AddMemberToConsumer(member);
                }
            }
        }

        if (fieldDecl.Parent != null && fieldDecl.Parent.IsUnion)
        {
            fieldDeclarationSyntax = fieldDeclarationSyntax.AddAttributeLists(
                SyntaxFactory.AttributeList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("FieldOffset"),
                            SyntaxFactory.AttributeArgumentList(SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.AttributeArgument(
                                        SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                            SyntaxFactory.Literal(0))
                                    )
                                )
                            )
                        )
                    )
                )
            );
        }

        return fieldDeclarationSyntax;
    }
}

﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ClangSharp;
using ClangSharp.Interop;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Type = ClangSharp.Type;

namespace ZstdSharp.Generator.CodeGenerator;

internal partial class CodeGenerator
{
    private SyntaxNode? VisitExpr(Expr expr)
    {
        _typeCaster.Visit(expr);

        var node = expr switch
        {
            ImplicitCastExpr implicitCastExpr => VisitImplicitCastExpr(implicitCastExpr),
            BinaryOperator binaryOperator => VisitBinaryOperator(binaryOperator),
            UnaryExprOrTypeTraitExpr unaryExprOrTypeTraitExpr =>
                VisitUnaryExprOrTypeTraitExpr(unaryExprOrTypeTraitExpr),
            CallExpr callExpr => VisitCallExpr(callExpr),
            ParenExpr parenExpr => VisitParenExpr(parenExpr),
            UnaryOperator unaryOperator => VisitUnaryOperator(unaryOperator),
            DeclRefExpr declRefExpr => VisitDeclRefExpr(declRefExpr),
            CStyleCastExpr cStyleCastExpr => VisitCStyleCastExpr(cStyleCastExpr),
            ArraySubscriptExpr arraySubscriptExpr => VisitArraySubscriptExpr(arraySubscriptExpr),
            InitListExpr initListExpr => VisitInitListExpr(initListExpr),
            MemberExpr memberExpr => VisitMemberExpr(memberExpr),
            ConditionalOperator conditionalOperator => VisitConditionalOperator(conditionalOperator),
            IntegerLiteral integerLiteral => VisitIntegerLiteral(integerLiteral),
            StringLiteral stringLiteral => VisitStringLiteral(stringLiteral),
            FloatingLiteral floatingLiteral => VisitFloatingLiteral(floatingLiteral),
            _ => UnsupportedNodeType(expr)
        };

        // x[a][b] -> x[a, b]
        if (Config.ConvertNestedArraysToMultidimensional && node is ElementAccessExpressionSyntax elementAccessExpression)
        {
            var argumentList = elementAccessExpression.ArgumentList;
            var argumentSyntaxList = new List<ArgumentSyntax>(argumentList.Arguments);
            var p = elementAccessExpression.Expression;

            while (p is ElementAccessExpressionSyntax pElementAccessExpression)
            {
                var pArgumentList = pElementAccessExpression.ArgumentList;
                argumentSyntaxList.InsertRange(0, pArgumentList.Arguments);
                p = pElementAccessExpression.Expression;
            }

            if (argumentSyntaxList.Count > 1)
            {
                node = SyntaxFactory.ElementAccessExpression(p,
                    SyntaxFactory.BracketedArgumentList(SyntaxFactory.SeparatedList(argumentSyntaxList)));
            }
        }

        if (node is ExpressionSyntax)
        {
            var casts = _typeCaster.GetCasts(expr);
            if (casts != null)
            {
                foreach (var cast in casts)
                {
                    if (node is ExpressionSyntax expressionSyntax)
                    {
                        node = cast.Apply(expr, this, expressionSyntax);
                    } else
                    {
                        Report(DiagnosticLevel.Error, "Invalid cast expression");
                        break;
                    }
                }
            }

            if (node is InvocationExpressionSyntax { Expression: IdentifierNameSyntax identifierNameSyntax} invocationExpressionSyntax)
            {
                var callReplacement = _callReplacer.GetCallReplacement(identifierNameSyntax.Identifier.Text);
                if (callReplacement != null)
                {
                    if (callReplacement.UsingDirective != null)
                    {
                        AddUsing(callReplacement.UsingDirective);
                    }

                    return callReplacement.Apply(invocationExpressionSyntax, expr, this);
                }
            }
        }

        if (node is ExpressionSyntax expression && IsVoidExpr(expression))
        {
            return null;
        }

        return node;
    }

    private SyntaxNode VisitConditionalOperator(ConditionalOperator conditionalOperator)
    {
        var cond = Visit<ExpressionSyntax>(conditionalOperator.Cond)!;
        var whenTrue = Visit<ExpressionSyntax>(conditionalOperator.TrueExpr)!;
        var whenFalse = Visit<ExpressionSyntax>(conditionalOperator.FalseExpr)!;
        var constantCond = TreeHelper.GetValueOfType<bool>(cond);

        if (constantCond.HasValue)
        {
            if (constantCond.Value && IsPureExpr(whenFalse))
            {
                return whenTrue;
            }

            if (!constantCond.Value && IsPureExpr(whenTrue))
            {
                return whenFalse;
            }
        }

        return SyntaxFactory.ConditionalExpression(cond, whenTrue, whenFalse);
    }

    private SyntaxNode VisitMemberExpr(MemberExpr memberExpr)
    {
        if (!memberExpr.IsImplicitAccess)
        {
            var type = memberExpr.Base switch
            {
                CXXThisExpr => null,
                DeclRefExpr declRefExpr => declRefExpr.Decl.Type.CanonicalType,
                _ => memberExpr.Base.Type.CanonicalType
            };

            var kind = type is PointerType or ReferenceType
                ? SyntaxKind.PointerMemberAccessExpression
                : SyntaxKind.SimpleMemberAccessExpression;

            // struct to class access
            if (kind == SyntaxKind.PointerMemberAccessExpression)
            {
                var cSharpType = GetRemappedCSharpType(memberExpr.Base, type!, out _, false);
                if (cSharpType is not PointerTypeSyntax)
                {
                    kind = SyntaxKind.SimpleMemberAccessExpression;
                }
            }

            return SyntaxFactory.MemberAccessExpression(kind, Visit<ExpressionSyntax>(memberExpr.Base)!, SyntaxFactory.IdentifierName(EscapeName(GetRemappedCursorName(memberExpr.MemberDecl))));
        }

        return SyntaxFactory.IdentifierName(EscapeName(GetRemappedCursorName(memberExpr.MemberDecl)));
    }

    private SyntaxNode? VisitInitListExpr(InitListExpr initListExpr)
    {
        return ForType(initListExpr, initListExpr.Type);

        ExpressionSyntax ForArrayType(InitListExpr innerInitListExpr, ArrayType arrayType)
        {
            var cSharpType = GetRemappedCSharpType(innerInitListExpr, arrayType, out _, false);

            var arrayTypeSyntax = (ArrayTypeSyntax)cSharpType;

            var size = -1L;
            var isDefaultZero = false;

            if (arrayType is ConstantArrayType constantArrayType)
            {
                size = constantArrayType.Size;

                arrayTypeSyntax = arrayTypeSyntax.WithRankSpecifiers(
                    SyntaxFactory.List(
                        new[]
                        {
                            SyntaxFactory.ArrayRankSpecifier(
                                SyntaxFactory.SingletonSeparatedList<ExpressionSyntax>(
                                    SyntaxFactory.LiteralExpression(
                                        SyntaxKind.NumericLiteralExpression,
                                        SyntaxFactory.Literal((int) size))))
                        }.Concat(arrayTypeSyntax.RankSpecifiers.Skip(1))
                    ));

                isDefaultZero = constantArrayType.ElementType.CanonicalType.IsIntegerType ||
                                     constantArrayType.ElementType.CanonicalType.IsPointerType;
            }

            return SyntaxFactory.ArrayCreationExpression(arrayTypeSyntax)
                .WithInitializer(SyntaxFactory.InitializerExpression(
                        SyntaxKind.ArrayInitializerExpression,
                        SyntaxFactory.SeparatedList(InitEnumerable())
                    )
                );

            IEnumerable<ExpressionSyntax> InitEnumerable()
            {
                var initEnd = innerInitListExpr.Inits.Count - 1;

                while (isDefaultZero && initEnd >= 0 && IsZeroConstant(innerInitListExpr.Inits[initEnd]))
                {
                    initEnd--;
                }

                initEnd++;

                for (var i = 0; i < initEnd; i++)
                {
                    var init = innerInitListExpr.Inits[i];
                    var expressionSyntax = Visit<ExpressionSyntax>(init);
                    if (expressionSyntax != null)
                    {
                        yield return expressionSyntax;
                    }
                }

                for (var i = initEnd; i < size; i++)
                {
                    yield return SyntaxFactory.LiteralExpression(
                        SyntaxKind.DefaultLiteralExpression,
                        SyntaxFactory.Token(SyntaxKind.DefaultKeyword)
                    );
                }
            }

            bool IsZeroConstant(Stmt stmt)
            {
                begin:
                if (stmt is ParenExpr parenExpr)
                {
                    stmt = parenExpr.SubExpr;
                    goto begin;
                }
                if (stmt is ImplicitCastExpr implicitCastExpr)
                {
                    stmt = implicitCastExpr.SubExpr;
                    goto begin;
                }

                return stmt is IntegerLiteral { Value: 0 };
            }
        }

        ExpressionSyntax ForRecordType(InitListExpr innerInitListExpr, RecordType recordType)
        {
            var type = innerInitListExpr.Type;
            var cSharpType = GetRemappedCSharpType(innerInitListExpr, type, out _);

            if (Config.AvoidObjectInitializationInStatic)
            {
                var varDecl = ReverseContext().OfType<VarDecl>().FirstOrDefault();
                if (varDecl != null && IsVarDeclField(varDecl) == true)
                {
                    var typeName = cSharpType.ToString();

                    if (_projectBuilder.TryGetTypeDeclaration(typeName, out var typeDeclaration, out var fileBuilder))
                    {
                        AddInitConstructor(typeName, typeDeclaration, recordType.Decl, fileBuilder);
                    }

                    return SyntaxFactory.ObjectCreationExpression(
                            SyntaxFactory.IdentifierName(typeName))
                        .WithArgumentList(
                            SyntaxFactory.ArgumentList(
                                SyntaxFactory.SeparatedList(ArgsEnumerable())
                                )
                            );
                }
            }

            return SyntaxFactory.ObjectCreationExpression(cSharpType)
                .WithInitializer(
                    SyntaxFactory.InitializerExpression(
                        SyntaxKind.ObjectInitializerExpression,
                        SyntaxFactory.SeparatedList(InitEnumerable())
                    )
                );

            IEnumerable<(int, Stmt)> GetInits()
            {
                for (var i = 0; i < innerInitListExpr.Inits.Count; i++)
                {
                    var init = innerInitListExpr.Inits[i];
                    if (init is ImplicitValueInitExpr)
                    {
                        continue;
                    }

                    yield return (i, init);
                }
            }

            IEnumerable<ExpressionSyntax> InitEnumerable()
            {
                var decl = recordType.Decl;
                foreach (var (i, init) in GetInits())
                {
                    var fieldName = GetRemappedCursorName(decl.Fields[i]);
                    yield return SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(fieldName),
                        Visit<ExpressionSyntax>(init)!);
                }
            }

            IEnumerable<ArgumentSyntax> ArgsEnumerable()
            {
                var decl = recordType.Decl;
                foreach (var (i, init) in GetInits())
                {
                    var fieldName = GetRemappedCursorName(decl.Fields[i]);
                    yield return SyntaxFactory.Argument(Visit<ExpressionSyntax>(init)!)
                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName(fieldName)));
                }
            }
        }

        ExpressionSyntax? ForType(InitListExpr innerInitListExpr, Type type)
        {
            type = UnwrapElaborated(type);

            if (type is ArrayType arrayType)
            {
                return ForArrayType(innerInitListExpr, arrayType);
            }

            if (type is RecordType recordType)
            {
                return ForRecordType(innerInitListExpr, recordType);
            }

            if (UnwrapElaborated(type) is TypedefType typedefType)
            {
                return ForType(innerInitListExpr, typedefType.Decl.UnderlyingType);
            }

            Report(DiagnosticLevel.Error, $"Unsupported init list expression type: {type.KindSpelling}");
            return null;
        }
    }

    // small arrays can use e0..e7 index access
    private bool CanUseInlineArray(long size) => size > 8;

    private SyntaxNode VisitArraySubscriptExpr(ArraySubscriptExpr arraySubscriptExpr)
    {
        (Expr baseExpr, Expr idxExpr) = ExtractArraySubscriptExpr(arraySubscriptExpr);
        var baseExpression = Visit<ExpressionSyntax>(baseExpr)!;
        // s->f[1] -> s->f.e1
        if (IsArtificialFixedBufferAccess(baseExpr, out var subExpression, out var size))
        {
            var evalResult = idxExpr.Handle.Evaluate;
            if (evalResult.Kind == CXEvalResultKind.CXEval_Int)
            {
                var index = evalResult.AsLongLong;
                if (index == 0 || (index > 0 && !CanUseInlineArray(size)))
                {
                    return SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, Visit<ExpressionSyntax>(subExpression)!,
                                SyntaxFactory.IdentifierName($"e{index}"));
                }
            }
        }

        return SyntaxFactory.ElementAccessExpression(baseExpression,
            SyntaxFactory.BracketedArgumentList(
                SyntaxFactory.SingletonSeparatedList(
                    SyntaxFactory.Argument(Visit<ExpressionSyntax>(idxExpr)!))));
    }

    private SyntaxNode? VisitCStyleCastExpr(CStyleCastExpr cStyleCastExpr)
    {
        var type = cStyleCastExpr.Type;
        var cSharpType = GetRemappedCSharpType(cStyleCastExpr, type, out _);

        if (cStyleCastExpr.CastKind == CX_CastKind.CX_CK_NullToPointer)
        {
            return SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression);
        }

        return CreateCast(cStyleCastExpr.SubExpr, cSharpType, Visit<ExpressionSyntax>(cStyleCastExpr.SubExpr)!);
    }

    internal ExpressionSyntax CreateCast(Expr expr, TypeSyntax type, ExpressionSyntax expression)
    {
        var castExpressionSyntax =
            SyntaxFactory.CastExpression(type, SyntaxFactory.ParenthesizedExpression(expression));

        // unchecked
        if (type.ToString() is "nuint" or "uint" or "ushort" or "byte")
        {
            var evalResult = expr.Handle.Evaluate;
            if (evalResult.Kind == CXEvalResultKind.CXEval_Int)
            {
                var signedValue = evalResult.AsLongLong;
                if (signedValue < 0)
                {
                    return SyntaxFactory.CheckedExpression(SyntaxKind.UncheckedExpression, castExpressionSyntax);
                }
            }
        }

        // (byte*)"..." => (byte*)(GetArrayPointer(new byte[] { ..., 0 }))
        if (expression is LiteralExpressionSyntax literalExpressionSyntax &&
            literalExpressionSyntax.Kind() == SyntaxKind.StringLiteralExpression &&
            type is PointerTypeSyntax pointerTypeSyntax && pointerTypeSyntax.ElementType.ToString() == "byte" &&
            literalExpressionSyntax.Token.Value is string stringValue)
        {
            var bytes = Encoding.UTF8.GetBytes(stringValue + '\0');
            var name = $"stringToByte_{BitConverter.ToString(bytes).Replace("-", "_")}";
            if (_projectBuilder.AddGeneratedType(name))
            {
                AddMethodsMember(CreateArrayField(name, GetType("byte"), bytes.Select(x =>
                    SyntaxFactory.LiteralExpression(
                        SyntaxKind.NumericLiteralExpression,
                        SyntaxFactory.Literal(x)))));
            }

            castExpressionSyntax = SyntaxFactory.CastExpression(type, SyntaxFactory.IdentifierName(name));
        }

        return castExpressionSyntax;
    }

    private SyntaxNode VisitDeclRefExpr(DeclRefExpr declRefExpr)
    {
        var prefix = "";
        if (declRefExpr.Decl is EnumConstantDecl enumConstantDecl && declRefExpr.DeclContext != enumConstantDecl.DeclContext && enumConstantDecl.DeclContext is NamedDecl namedDecl)
        {
            var enumName = EscapeName(GetRemappedCursorName(namedDecl));
            prefix = $"{enumName}.";
        }

        var name = GetRemappedCursorName(declRefExpr.Decl);
        return SyntaxFactory.IdentifierName(prefix + EscapeName(name));
    }

    private SyntaxNode? VisitUnaryOperator(UnaryOperator unaryOperator)
    {
        var kind = GetUnaryKind(unaryOperator.Opcode);
        if (kind != null)
        {
            var subExpression = Visit<ExpressionSyntax>(unaryOperator.SubExpr);
            if (unaryOperator.IsPostfix)
            {
                return SyntaxFactory.PostfixUnaryExpression((SyntaxKind) kind,
                    subExpression!);
            }

            if (kind == SyntaxKind.AddressOfExpression)
            {
                var cSharpType = GetRemappedCSharpType(unaryOperator.SubExpr, unaryOperator.SubExpr.Type, out var arrayConvertedToPointer);

                // pointer to static local var
                if (unaryOperator.SubExpr is DeclRefExpr {Decl: VarDecl {StorageClass: CX_StorageClass.CX_SC_Static, HasInit: true} varDecl})
                {
                    var initExpression = Visit<ExpressionSyntax>(varDecl.Init)!;

                    var name = $"static_{varDecl.Name}";
                    var pointerType = cSharpType;
                    if (_projectBuilder.AddGeneratedType(name))
                    {
                        IEnumerable<ExpressionSyntax> values = new[] { initExpression };
                        if (TreeHelper.GetValue(initExpression, out var value) && value is 0 &&
                            cSharpType.ToString() is "nuint" or "nint")
                        {
                            // extend zero to 64 bits (8 bytes) to use array optimization
                            cSharpType = GetType("byte");
                            values = Enumerable.Repeat(initExpression, 8);
                        }

                        AddMethodsMember(CreateArrayField(name, cSharpType, values, pointerType));
                    }

                    // &x -> &static_x[0]
                    return SyntaxFactory.PrefixUnaryExpression((SyntaxKind) kind, SyntaxFactory.ElementAccessExpression(
                        SyntaxFactory.IdentifierName(name),
                        SyntaxFactory.BracketedArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(
                                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                        SyntaxFactory.Literal(0)))))));
                }

                // dereference
                if (IsStructToClass(cSharpType.ToString()))
                {
                    return subExpression;
                }

                // &array -> &array[0]
                if (arrayConvertedToPointer || cSharpType is ArrayTypeSyntax)
                {
                    // exclude crafted fixed buffer because &array[0] is not fixed, &array is equivalent to &array[0] here
                    var isArtificialFixedSizedBuffer = false;
                    if (unaryOperator.SubExpr is MemberExpr)
                    {
                        var innerType = GetElementType(cSharpType, out _);
                        isArtificialFixedSizedBuffer =
                            innerType != null && !IsSupportedFixedSizedBufferType(innerType.ToString());
                    }

                    if (!isArtificialFixedSizedBuffer)
                    {
                        return SyntaxFactory.PrefixUnaryExpression(SyntaxKind.AddressOfExpression,
                            SyntaxFactory.ElementAccessExpression(
                                subExpression!,
                                SyntaxFactory.BracketedArgumentList(
                                    SyntaxFactory.SingletonSeparatedList(
                                        SyntaxFactory.Argument(SyntaxFactory.LiteralExpression(
                                            SyntaxKind.NumericLiteralExpression,
                                            SyntaxFactory.Literal(0)))))));
                    }
                }
            }

            return SyntaxFactory.PrefixUnaryExpression((SyntaxKind) kind,
                subExpression!);
        }

        Report(DiagnosticLevel.Error, $"Unknown kind {unaryOperator.Opcode} {unaryOperator.OpcodeStr}");
        return null;
    }

    private SyntaxNode? VisitParenExpr(ParenExpr parenExpr)
    {
        var subExpr = Visit<ExpressionSyntax>(parenExpr.SubExpr);
        return subExpr != null ? SyntaxFactory.ParenthesizedExpression(subExpr) : null;
    }

    private SyntaxNode VisitCallExpr(CallExpr callExpr)
    {
        var calleeExpression = Visit<ExpressionSyntax>(callExpr.Callee)!;
        // callee(X) -> ((delegate*)callee)(X)
        if (Config.HideFunctionPointers)
        {
            var functionPointerType = GetCalleeFunctionProtoType(callExpr.Callee);
            if (functionPointerType != null)
            {
                calleeExpression = SyntaxFactory.ParenthesizedExpression(CreateCast(callExpr,
                    GetFunctionPointerType(callExpr.Callee, functionPointerType), calleeExpression));
            }
        }

        return SyntaxFactory.InvocationExpression(calleeExpression,
            SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(callExpr.Args.Select(arg =>
                SyntaxFactory.Argument(Visit<ExpressionSyntax>(arg)!)))));
    }

    private ExpressionSyntax GetTypeSizeOf(Cursor cursor, Type type)
    {
        switch (type.CanonicalType)
        {
            case ConstantArrayType constantArrayType:
                return SyntaxFactory.BinaryExpression(SyntaxKind.MultiplyExpression,
                    GetTypeSizeOf(cursor, constantArrayType.ElementType),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                        SyntaxFactory.Literal((int) Math.Max(constantArrayType.Size, 1))));
            default:
                var typeName = GetRemappedCSharpType(cursor, type, out _);
                if (Config.VariableSizeTypes.TryGetValue(typeName.ToString(), out var variableSizeType))
                {
                    return variableSizeType.GetSizeOf(typeName);
                }

                return SyntaxFactory.SizeOfExpression(typeName);
        }
    }

    private ExpressionSyntax GetTypeAlignOf(Cursor cursor, Type type)
    {
        if (type is ElaboratedType elaboratedType)
        {
            return GetTypeAlignOf(cursor, elaboratedType.NamedType);
        }

        if (type is TypedefType typedefType)
        {
            return GetTypeAlignOf(cursor, typedefType.Decl.UnderlyingType);
        }

        if (type is ArrayType arrayType)
        {
            return GetTypeAlignOf(cursor, arrayType.ElementType);
        }

        if (type is RecordType recordType)
        {
            return recordType.Decl.Fields
                       .Select(field => GetTypeAlignOf(cursor, field.Type))
                       .DistinctBy(e => e.ToString())
                       .Aggregate((ExpressionSyntax?) null,
                           (accumulate, expression) => accumulate == null
                               ? expression
                               : SyntaxFactory.InvocationExpression(
                                       SyntaxFactory.IdentifierName("Math.Max"))
                                   .WithArgumentList(SyntaxFactory.ArgumentList(
                                       SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                           new[]
                                           {
                                               SyntaxFactory.Argument(accumulate), SyntaxFactory.Argument(expression)
                                           })))) ??
                   SyntaxFactory.LiteralExpression(0);
        }

        return GetTypeSizeOf(cursor, type);
    }

    private SyntaxNode? VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr unaryExprOrTypeTraitExpr)
    {
        var argumentType = unaryExprOrTypeTraitExpr.TypeOfArgument;

        switch (unaryExprOrTypeTraitExpr.Kind)
        {
            case CX_UnaryExprOrTypeTrait.CX_UETT_AlignOf:
            case CX_UnaryExprOrTypeTrait.CX_UETT_PreferredAlignOf:
                return GetTypeAlignOf(unaryExprOrTypeTraitExpr, argumentType);

            case CX_UnaryExprOrTypeTrait.CX_UETT_SizeOf:
                return GetTypeSizeOf(unaryExprOrTypeTraitExpr, argumentType);
            default:
                Report(DiagnosticLevel.Error, $"Unknown UnaryExprOrTypeTraitExpr kind {unaryExprOrTypeTraitExpr.Kind}");
                return null;
        }
    }

    private static SyntaxKind? GetBinaryKind(CXBinaryOperatorKind operatorKind)
    {
        return operatorKind switch
        {
            CXBinaryOperatorKind.CXBinaryOperator_Add => SyntaxKind.AddExpression,
            CXBinaryOperatorKind.CXBinaryOperator_Sub => SyntaxKind.SubtractExpression,
            CXBinaryOperatorKind.CXBinaryOperator_Mul => SyntaxKind.MultiplyExpression,
            CXBinaryOperatorKind.CXBinaryOperator_Div => SyntaxKind.DivideExpression,
            CXBinaryOperatorKind.CXBinaryOperator_Rem => SyntaxKind.ModuloExpression,
            CXBinaryOperatorKind.CXBinaryOperator_Shl => SyntaxKind.LeftShiftExpression,
            CXBinaryOperatorKind.CXBinaryOperator_Shr => SyntaxKind.RightShiftExpression,
            CXBinaryOperatorKind.CXBinaryOperator_LOr => SyntaxKind.LogicalOrExpression,
            CXBinaryOperatorKind.CXBinaryOperator_LAnd => SyntaxKind.LogicalAndExpression,
            CXBinaryOperatorKind.CXBinaryOperator_Or => SyntaxKind.BitwiseOrExpression,
            CXBinaryOperatorKind.CXBinaryOperator_And => SyntaxKind.BitwiseAndExpression,
            CXBinaryOperatorKind.CXBinaryOperator_Xor => SyntaxKind.ExclusiveOrExpression,
            CXBinaryOperatorKind.CXBinaryOperator_EQ => SyntaxKind.EqualsExpression,
            CXBinaryOperatorKind.CXBinaryOperator_NE => SyntaxKind.NotEqualsExpression,
            CXBinaryOperatorKind.CXBinaryOperator_LT => SyntaxKind.LessThanExpression,
            CXBinaryOperatorKind.CXBinaryOperator_LE => SyntaxKind.LessThanOrEqualExpression,
            CXBinaryOperatorKind.CXBinaryOperator_GT => SyntaxKind.GreaterThanExpression,
            CXBinaryOperatorKind.CXBinaryOperator_GE => SyntaxKind.GreaterThanOrEqualExpression,
            _ => null
        };
    }

    private static SyntaxKind? GetAssignmentKind(CXBinaryOperatorKind operatorKind)
    {
        return operatorKind switch
        {
            CXBinaryOperatorKind.CXBinaryOperator_Assign => SyntaxKind.SimpleAssignmentExpression,
            CXBinaryOperatorKind.CXBinaryOperator_MulAssign => SyntaxKind.MultiplyAssignmentExpression,
            CXBinaryOperatorKind.CXBinaryOperator_DivAssign => SyntaxKind.DivideAssignmentExpression,
            CXBinaryOperatorKind.CXBinaryOperator_RemAssign => SyntaxKind.ModuloAssignmentExpression,
            CXBinaryOperatorKind.CXBinaryOperator_AddAssign => SyntaxKind.AddAssignmentExpression,
            CXBinaryOperatorKind.CXBinaryOperator_SubAssign => SyntaxKind.SubtractAssignmentExpression,
            CXBinaryOperatorKind.CXBinaryOperator_ShlAssign => SyntaxKind.LeftShiftAssignmentExpression,
            CXBinaryOperatorKind.CXBinaryOperator_ShrAssign => SyntaxKind.RightShiftAssignmentExpression,
            CXBinaryOperatorKind.CXBinaryOperator_AndAssign => SyntaxKind.AndAssignmentExpression,
            CXBinaryOperatorKind.CXBinaryOperator_XorAssign => SyntaxKind.ExclusiveOrAssignmentExpression,
            CXBinaryOperatorKind.CXBinaryOperator_OrAssign => SyntaxKind.OrAssignmentExpression,
            _ => null
        };
    }

    private static SyntaxKind? GetUnaryKind(CXUnaryOperatorKind operatorKind)
    {
        return operatorKind switch
        {
            CXUnaryOperatorKind.CXUnaryOperator_PreInc => SyntaxKind.PreIncrementExpression,
            CXUnaryOperatorKind.CXUnaryOperator_PostInc => SyntaxKind.PostIncrementExpression,
            CXUnaryOperatorKind.CXUnaryOperator_PreDec => SyntaxKind.PreDecrementExpression,
            CXUnaryOperatorKind.CXUnaryOperator_PostDec => SyntaxKind.PostDecrementExpression,
            CXUnaryOperatorKind.CXUnaryOperator_AddrOf => SyntaxKind.AddressOfExpression,
            CXUnaryOperatorKind.CXUnaryOperator_Deref => SyntaxKind.PointerIndirectionExpression,
            CXUnaryOperatorKind.CXUnaryOperator_Plus => SyntaxKind.UnaryPlusExpression,
            CXUnaryOperatorKind.CXUnaryOperator_Minus => SyntaxKind.UnaryMinusExpression,
            CXUnaryOperatorKind.CXUnaryOperator_Not => SyntaxKind.BitwiseNotExpression,
            CXUnaryOperatorKind.CXUnaryOperator_LNot => SyntaxKind.LogicalNotExpression,
            _ => null
        };
    }

    private SyntaxNode? VisitBinaryOperator(BinaryOperator binaryOperator)
    {
        if (binaryOperator.Opcode == CXBinaryOperatorKind.CXBinaryOperator_Comma)
        {
            var left = Visit<ExpressionSyntax>(binaryOperator.LHS);
            if (left != null)
            {
                AddStatementToConsumer(SyntaxFactory.ExpressionStatement(left));
            }

            return Visit<ExpressionSyntax>(binaryOperator.RHS)!;
        }

        var kind = GetBinaryKind(binaryOperator.Opcode);
        if (kind != null)
        {
            var left = Visit<ExpressionSyntax>(binaryOperator.LHS);
            var right = Visit<ExpressionSyntax>(binaryOperator.RHS);
            if (left == null || right == null)
            {
                return null;
            }

            ExpressionSyntax expressionSyntax = SyntaxFactory.BinaryExpression((SyntaxKind)kind, left, right);

            // 0UL - 1 -> unchecked
            if (binaryOperator.Opcode == CXBinaryOperatorKind.CXBinaryOperator_Sub)
            {
                if (binaryOperator.LHS.Type.CanonicalType.Kind == CXTypeKind.CXType_ULongLong &&
                    binaryOperator.RHS.Type.CanonicalType.Kind == CXTypeKind.CXType_ULongLong)
                {
                    var lhs = GetExprAsWritten(binaryOperator.LHS, true);
                    var rhs = GetExprAsWritten(binaryOperator.RHS, true);

                    if (lhs is IntegerLiteral lhsIntegerLiteral && rhs is IntegerLiteral rhsIntegerLiteral &&
                        lhsIntegerLiteral.Value < rhsIntegerLiteral.Value)
                    {
                        expressionSyntax = SyntaxFactory.CheckedExpression(SyntaxKind.UncheckedExpression, expressionSyntax);
                    }
                }
            }

            if (binaryOperator.Opcode == CXBinaryOperatorKind.CXBinaryOperator_LAnd)
            {
                var leftValue = TreeHelper.GetValueOfType<bool>(left);
                var rightValue = TreeHelper.GetValueOfType<bool>(right);
                if (leftValue != null)
                {
                    // true && x -> x
                    if (leftValue.Value)
                    {
                        return right;
                    }

                    // false && x -> false
                    return SyntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression);
                }

                if (rightValue != null)
                {
                    // x && true -> x
                    if (rightValue.Value)
                    {
                        return left;
                    }

                    // x && false -> false
                    return SyntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression);
                }
            }

            if (binaryOperator.Opcode == CXBinaryOperatorKind.CXBinaryOperator_LOr)
            {
                var leftValue = TreeHelper.GetValueOfType<bool>(left);
                var rightValue = TreeHelper.GetValueOfType<bool>(right);
                if (leftValue != null)
                {
                    // true || x -> true
                    if (leftValue.Value)
                    {
                        return SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression);
                    }

                    // false || x -> x
                    return right;
                }

                if (rightValue != null)
                {
                    // x || true -> true
                    if (rightValue.Value)
                    {
                        return SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression);
                    }

                    // x || false -> x
                    return left;
                }
            }
            return expressionSyntax;
        }

        var assignmentKind = GetAssignmentKind(binaryOperator.Opcode);
        if (assignmentKind != null)
        {
            var left = Visit<ExpressionSyntax>(binaryOperator.LHS);
            var right = Visit<ExpressionSyntax>(binaryOperator.RHS);
            if (left == null || right == null)
            {
                return null;
            }

            return SyntaxFactory.AssignmentExpression((SyntaxKind) assignmentKind, left, right);
        }

        Report(DiagnosticLevel.Error, $"Unknown kind {binaryOperator.Opcode} {binaryOperator.OpcodeStr}");
        return null;
    }

    private SyntaxNode VisitImplicitCastExpr(ImplicitCastExpr implicitCastExpr)
    {
        if (implicitCastExpr.CastKind == CX_CastKind.CX_CK_NullToPointer)
        {
            return SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression);
        }

        var subExpression = Visit<ExpressionSyntax>(implicitCastExpr.SubExpr)!;
        // ArtificialFixedBuffer access: s->f1 -> (&s->f1.e0)
        if (IsArtificialFixedBufferAccess(implicitCastExpr, out _, out _))
        {
            subExpression = SyntaxFactory.ParenthesizedExpression(
                SyntaxFactory.PrefixUnaryExpression(SyntaxKind.AddressOfExpression,
                    SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, subExpression,
                        SyntaxFactory.IdentifierName("e0"))));
        }

        return subExpression;
    }

    private static SyntaxNode VisitFloatingLiteral(FloatingLiteral floatingLiteral)
    {
        return floatingLiteral.Type.Kind switch
        {
            CXTypeKind.CXType_Float => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal((float) floatingLiteral.ValueAsApproximateDouble)),
            CXTypeKind.CXType_Double => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(floatingLiteral.ValueAsApproximateDouble)),
            _ => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(floatingLiteral.ValueAsApproximateDouble))
        };
    }

    private static SyntaxNode VisitStringLiteral(StringLiteral stringLiteral)
    {
        return SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression,
            SyntaxFactory.Literal(stringLiteral.String));
    }

    private static SyntaxNode VisitIntegerLiteral(IntegerLiteral integerLiteral)
    {
        var valueString = integerLiteral.ValueString;

        if (valueString.EndsWith("l", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^1];
        }
        else if (valueString.EndsWith("ui8", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^3];
        }
        else if (valueString.EndsWith("i8", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^2];
        }
        else if (valueString.EndsWith("ui16", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^4];
        }
        else if (valueString.EndsWith("i16", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^3];
        }
        else if (valueString.EndsWith("i32", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^3];
        }
        else if (valueString.EndsWith("i64", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^3] + "L";
        } else if (valueString.EndsWith("llu", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^3] + "UL";
        }

        if (valueString.EndsWith("ul", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^2] + "UL";
        }
        else if (valueString.EndsWith("l", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^1] + "L";
        }
        else if (valueString.EndsWith("u", StringComparison.OrdinalIgnoreCase))
        {
            valueString = valueString[..^1] + "U";
        }

        return integerLiteral.Type.Kind switch
        {
            CXTypeKind.CXType_Int => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(valueString, (int) integerLiteral.Value)),
            CXTypeKind.CXType_Long => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(valueString, (int) integerLiteral.Value)),
            CXTypeKind.CXType_UInt => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(valueString, (uint) integerLiteral.Value)),
            CXTypeKind.CXType_ULong => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(valueString, (uint) integerLiteral.Value)),
            CXTypeKind.CXType_ULongLong => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(valueString, (ulong) integerLiteral.Value)),
            CXTypeKind.CXType_LongLong => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(valueString, integerLiteral.Value)),
            _ => SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(valueString, integerLiteral.Value))
        };
    }
}

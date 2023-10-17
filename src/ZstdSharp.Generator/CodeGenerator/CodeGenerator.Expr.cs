using System;
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
        _typeCaster.Visit(expr, _context);

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

                    return callReplacement.Apply(invocationExpressionSyntax);
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
        return SyntaxFactory.ConditionalExpression(Visit<ExpressionSyntax>(conditionalOperator.Cond)!,
            Visit<ExpressionSyntax>(conditionalOperator.TrueExpr)!,
            Visit<ExpressionSyntax>(conditionalOperator.FalseExpr)!);
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

            return SyntaxFactory.ObjectCreationExpression(cSharpType)
                .WithInitializer(
                    SyntaxFactory.InitializerExpression(
                        SyntaxKind.ObjectInitializerExpression,
                        SyntaxFactory.SeparatedList(InitEnumerable())
                    )
                );

            IEnumerable<ExpressionSyntax> InitEnumerable()
            {
                var decl = recordType.Decl;

                for (int i = 0; i < innerInitListExpr.Inits.Count; i++)
                {
                    var init = innerInitListExpr.Inits[i];

                    if (init is ImplicitValueInitExpr)
                    {
                        continue;
                    }

                    var fieldName = GetRemappedCursorName(decl.Fields[i]);

                    yield return SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(fieldName),
                        Visit<ExpressionSyntax>(init)!);
                }
            }
        }

        ExpressionSyntax? ForType(InitListExpr innerInitListExpr, Type type)
        {
            if (type is ArrayType arrayType)
            {
                return ForArrayType(innerInitListExpr, arrayType);
            }

            if (type is ElaboratedType elaboratedType)
            {
                return ForType(innerInitListExpr, elaboratedType.NamedType);
            }

            if (type is RecordType recordType)
            {
                return ForRecordType(innerInitListExpr, recordType);
            }

            if (type is TypedefType typedefType)
            {
                return ForType(innerInitListExpr, typedefType.Decl.UnderlyingType);
            }

            Report(DiagnosticLevel.Error, $"Unsupported init list expression type: {type.KindSpelling}");
            return null;
        }
    }

    private SyntaxNode VisitArraySubscriptExpr(ArraySubscriptExpr arraySubscriptExpr)
    {
        (Expr baseExpr, Expr idxExpr) = ExtractArraySubscriptExpr(arraySubscriptExpr);
        var baseExpression = Visit<ExpressionSyntax>(baseExpr)!;
        // s->f[1] -> s->f.e1
        if (IsArtificialFixedBufferAccess(baseExpr, out var subExpression))
        {
            var evalResult = idxExpr.Handle.Evaluate;
            if (evalResult.Kind == CXEvalResultKind.CXEval_Int)
            {
                var signedValue = evalResult.AsLongLong;
                if (signedValue >= 0)
                {
                    return SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, Visit<ExpressionSyntax>(subExpression)!,
                                SyntaxFactory.IdentifierName($"e{signedValue}"));
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
        if (new[] {"nuint", "uint", "ushort", "byte"}.Contains(type.ToString()))
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
                AddMethodsMember(CreateByteArrayField(name, bytes));
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
            if (unaryOperator.IsPostfix)
            {
                return SyntaxFactory.PostfixUnaryExpression((SyntaxKind) kind,
                    Visit<ExpressionSyntax>(unaryOperator.SubExpr)!);
            }

            if (kind == SyntaxKind.AddressOfExpression)
            {
                var cSharpType = GetRemappedCSharpType(unaryOperator.SubExpr, unaryOperator.SubExpr.Type, out var arrayConvertedToPointer);

                // dereference
                if (IsStructToClass(cSharpType.ToString()))
                {
                    return Visit<ExpressionSyntax>(unaryOperator.SubExpr);
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
                                Visit<ExpressionSyntax>(unaryOperator.SubExpr)!,
                                SyntaxFactory.BracketedArgumentList(
                                    SyntaxFactory.SingletonSeparatedList(
                                        SyntaxFactory.Argument(SyntaxFactory.LiteralExpression(
                                            SyntaxKind.NumericLiteralExpression,
                                            SyntaxFactory.Literal(0)))))));
                    }
                }
            }

            return SyntaxFactory.PrefixUnaryExpression((SyntaxKind) kind,
                Visit<ExpressionSyntax>(unaryOperator.SubExpr)!);
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
                calleeExpression = SyntaxFactory.ParenthesizedExpression(CreateCast(callExpr, GetFunctionPointerType(callExpr.Callee, functionPointerType), calleeExpression));
            }
        }

        return SyntaxFactory.InvocationExpression(calleeExpression,
            SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(callExpr.Args.Select(arg =>
                SyntaxFactory.Argument(Visit<ExpressionSyntax>(arg)!)))));
    }

    private ExpressionSyntax GetTypeSizeOf(Cursor cursor, Type type)
    {
        return type.CanonicalType switch
        {
            ConstantArrayType constantArrayType => SyntaxFactory.BinaryExpression(SyntaxKind.MultiplyExpression,
                GetTypeSizeOf(cursor, constantArrayType.ElementType),
                SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    SyntaxFactory.Literal((int) Math.Max(constantArrayType.Size, 1)))),
            _ => SyntaxFactory.SizeOfExpression(GetRemappedCSharpType(cursor, type, out _))
        };
    }

    private SyntaxNode? VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr unaryExprOrTypeTraitExpr)
    {
        var argumentType = unaryExprOrTypeTraitExpr.TypeOfArgument;

        switch (unaryExprOrTypeTraitExpr.Kind)
        {
            case CX_UnaryExprOrTypeTrait.CX_UETT_SizeOf:
            case CX_UnaryExprOrTypeTrait.CX_UETT_AlignOf:
            case CX_UnaryExprOrTypeTrait.CX_UETT_PreferredAlignOf:
                return GetTypeSizeOf(unaryExprOrTypeTraitExpr, argumentType);
            default:
                Report(DiagnosticLevel.Error, $"Unknown UnaryExprOrTypeTraitExpr kind {unaryExprOrTypeTraitExpr.Kind}");
                return null;
        }
    }

    private static SyntaxKind? GetBinaryKind(CX_BinaryOperatorKind operatorKind)
    {
        return operatorKind switch
        {
            CX_BinaryOperatorKind.CX_BO_Add => SyntaxKind.AddExpression,
            CX_BinaryOperatorKind.CX_BO_Sub => SyntaxKind.SubtractExpression,
            CX_BinaryOperatorKind.CX_BO_Mul => SyntaxKind.MultiplyExpression,
            CX_BinaryOperatorKind.CX_BO_Div => SyntaxKind.DivideExpression,
            CX_BinaryOperatorKind.CX_BO_Rem => SyntaxKind.ModuloExpression,
            CX_BinaryOperatorKind.CX_BO_Shl => SyntaxKind.LeftShiftExpression,
            CX_BinaryOperatorKind.CX_BO_Shr => SyntaxKind.RightShiftExpression,
            CX_BinaryOperatorKind.CX_BO_LOr => SyntaxKind.LogicalOrExpression,
            CX_BinaryOperatorKind.CX_BO_LAnd => SyntaxKind.LogicalAndExpression,
            CX_BinaryOperatorKind.CX_BO_Or => SyntaxKind.BitwiseOrExpression,
            CX_BinaryOperatorKind.CX_BO_And => SyntaxKind.BitwiseAndExpression,
            CX_BinaryOperatorKind.CX_BO_Xor => SyntaxKind.ExclusiveOrExpression,
            CX_BinaryOperatorKind.CX_BO_EQ => SyntaxKind.EqualsExpression,
            CX_BinaryOperatorKind.CX_BO_NE => SyntaxKind.NotEqualsExpression,
            CX_BinaryOperatorKind.CX_BO_LT => SyntaxKind.LessThanExpression,
            CX_BinaryOperatorKind.CX_BO_LE => SyntaxKind.LessThanOrEqualExpression,
            CX_BinaryOperatorKind.CX_BO_GT => SyntaxKind.GreaterThanExpression,
            CX_BinaryOperatorKind.CX_BO_GE => SyntaxKind.GreaterThanOrEqualExpression,
            _ => null
        };
    }

    private static SyntaxKind? GetAssignmentKind(CX_BinaryOperatorKind operatorKind)
    {
        return operatorKind switch
        {
            CX_BinaryOperatorKind.CX_BO_Assign => SyntaxKind.SimpleAssignmentExpression,
            CX_BinaryOperatorKind.CX_BO_MulAssign => SyntaxKind.MultiplyAssignmentExpression,
            CX_BinaryOperatorKind.CX_BO_DivAssign => SyntaxKind.DivideAssignmentExpression,
            CX_BinaryOperatorKind.CX_BO_RemAssign => SyntaxKind.ModuloAssignmentExpression,
            CX_BinaryOperatorKind.CX_BO_AddAssign => SyntaxKind.AddAssignmentExpression,
            CX_BinaryOperatorKind.CX_BO_SubAssign => SyntaxKind.SubtractAssignmentExpression,
            CX_BinaryOperatorKind.CX_BO_ShlAssign => SyntaxKind.LeftShiftAssignmentExpression,
            CX_BinaryOperatorKind.CX_BO_ShrAssign => SyntaxKind.RightShiftAssignmentExpression,
            CX_BinaryOperatorKind.CX_BO_AndAssign => SyntaxKind.AndAssignmentExpression,
            CX_BinaryOperatorKind.CX_BO_XorAssign => SyntaxKind.ExclusiveOrAssignmentExpression,
            CX_BinaryOperatorKind.CX_BO_OrAssign => SyntaxKind.OrAssignmentExpression,
            _ => null
        };
    }

    private static SyntaxKind? GetUnaryKind(CX_UnaryOperatorKind operatorKind)
    {
        return operatorKind switch
        {
            CX_UnaryOperatorKind.CX_UO_PreInc => SyntaxKind.PreIncrementExpression,
            CX_UnaryOperatorKind.CX_UO_PostInc => SyntaxKind.PostIncrementExpression,
            CX_UnaryOperatorKind.CX_UO_PreDec => SyntaxKind.PreDecrementExpression,
            CX_UnaryOperatorKind.CX_UO_PostDec => SyntaxKind.PostDecrementExpression,
            CX_UnaryOperatorKind.CX_UO_AddrOf => SyntaxKind.AddressOfExpression,
            CX_UnaryOperatorKind.CX_UO_Deref => SyntaxKind.PointerIndirectionExpression,
            CX_UnaryOperatorKind.CX_UO_Plus => SyntaxKind.UnaryPlusExpression,
            CX_UnaryOperatorKind.CX_UO_Minus => SyntaxKind.UnaryMinusExpression,
            CX_UnaryOperatorKind.CX_UO_Not => SyntaxKind.BitwiseNotExpression,
            CX_UnaryOperatorKind.CX_UO_LNot => SyntaxKind.LogicalNotExpression,
            _ => null
        };
    }

    private SyntaxNode? VisitBinaryOperator(BinaryOperator binaryOperator)
    {
        if (binaryOperator.Opcode == CX_BinaryOperatorKind.CX_BO_Comma)
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
            if (binaryOperator.Opcode == CX_BinaryOperatorKind.CX_BO_Sub)
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

    private SyntaxNode? VisitImplicitCastExpr(ImplicitCastExpr implicitCastExpr)
    {
        if (implicitCastExpr.CastKind == CX_CastKind.CX_CK_NullToPointer)
        {
            return SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression);
        }

        var subExpression = Visit<ExpressionSyntax>(implicitCastExpr.SubExpr)!;
        // ArtificialFixedBuffer access: s->f1 -> (&s->f1.e0)
        if (IsArtificialFixedBufferAccess(implicitCastExpr, out _))
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

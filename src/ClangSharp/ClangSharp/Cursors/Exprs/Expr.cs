// Copyright (c) .NET Foundation and Contributors. All Rights Reserved. Licensed under the MIT License (MIT). See License.md in the repository root for more information.

using System;
using System.Diagnostics;
using ClangSharp.Interop;

namespace ClangSharp;

public class Expr : ValueStmt
{
    private static readonly Func<Expr, Expr> s_ignoreImplicitCastsSingleStep = (e) => e is ImplicitCastExpr ice ? ice.SubExpr : e is FullExpr fe ? fe.SubExpr : e;

    private static readonly Func<Expr, Expr> s_ignoreImplicitSingleStep = (e) =>
    {
        var subE = s_ignoreImplicitCastsSingleStep(e);

        return subE != e ? subE : e is MaterializeTemporaryExpr mte ? mte.SubExpr : e is CXXBindTemporaryExpr bte ? bte.SubExpr : e;
    };

    private static readonly Func<Expr, Expr> s_ignoreParensSingleStep = (e) =>
    {
        if (e is ParenExpr pe)
        {
            return pe.SubExpr;
        }

        if (e is UnaryOperator uo)
        {
            if (uo.Opcode == CX_UnaryOperatorKind.CX_UO_Extension)
            {
                return uo.SubExpr;
            }
        }
        else if (e is GenericSelectionExpr gse)
        {
            if (!gse.IsResultDependent)
            {
                var resultExpr = gse.ResultExpr;
                Debug.Assert(resultExpr is not null);
                return resultExpr!;
            }
        }
        else if (e is ChooseExpr ce)
        {
            if (!ce.IsConditionDependent)
            {
                var chosenSubExpr = ce.ChosenSubExpr;
                Debug.Assert(chosenSubExpr is not null);
                return chosenSubExpr!;
            }
        }

        return e;
    };

    private readonly Lazy<Type> _type;

    private protected Expr(CXCursor handle, CXCursorKind expectedCursorKind, CX_StmtClass expectedStmtClass) : base(handle, expectedCursorKind, expectedStmtClass)
    {
        if (handle.StmtClass is > CX_StmtClass.CX_StmtClass_LastExpr or < CX_StmtClass.CX_StmtClass_FirstExpr)
        {
            throw new ArgumentOutOfRangeException(nameof(handle));
        }

        _type = new Lazy<Type>(() => TranslationUnit.GetOrCreate<Type>(Handle.Type));
    }

    public bool ContainsErrors => (Dependence & CX_ExprDependence.CX_ED_Error) != 0;

    public bool ContainsUnexpandedParameterPack => (Dependence & CX_ExprDependence.CX_ED_UnexpandedPack) != 0;

    public CX_ExprDependence Dependence => Handle.ExprDependence;

    public Expr IgnoreImplicit => IgnoreExprNodes(this, s_ignoreImplicitSingleStep);

    public Expr IgnoreParens => IgnoreExprNodes(this, s_ignoreParensSingleStep);

    public bool IsImplicitCXXThis
    {
        get
        {
            var e = this;

            while (true)
            {
                if (e is ParenExpr paren)
                {
                    e = paren.SubExpr;
                    continue;
                }

                if (e is ImplicitCastExpr ice)
                {
                    if (ice.CastKind is CX_CastKind.CX_CK_NoOp or CX_CastKind.CX_CK_LValueToRValue or CX_CastKind.CX_CK_DerivedToBase or CX_CastKind.CX_CK_UncheckedDerivedToBase)
                    {
                        e = ice.SubExpr;
                        continue;
                    }
                }

                if (e is UnaryOperator unOp)
                {
                    if (unOp.Opcode == CX_UnaryOperatorKind.CX_UO_Extension)
                    {
                        e = unOp.SubExpr;
                        continue;
                    }
                }

                if (e is MaterializeTemporaryExpr m)
                {
                    e = m.SubExpr;
                    continue;
                }

                break;
            }

            return e is CXXThisExpr self && self.IsImplicit;
        }
    }

    public bool IsInstantiationDependent => (Dependence & CX_ExprDependence.CX_ED_Instantiation) != 0;

    public bool IsTypeDependent => (Dependence & CX_ExprDependence.CX_ED_Type) != 0;

    public bool IsValueDependent => (Dependence & CX_ExprDependence.CX_ED_Value) != 0;

    public Type Type => _type.Value;

    private static Expr IgnoreExprNodes(Expr e, Func<Expr, Expr> fn)
    {
        Expr? lastE = null;

        while (e != lastE)
        {
            lastE = e;
            e = fn(e);
        }

        return e;
    }

    protected static Expr SkipImplicitTemporary(Expr e)
    {
        // Skip through reference binding to temporary.
        if (e is MaterializeTemporaryExpr materialize)
        {
            e = materialize.SubExpr;
        }

        // Skip any temporary bindings; they're implicit.
        if (e is CXXBindTemporaryExpr binder)
        {
            e = binder.SubExpr;
        }

        return e;
    }
}

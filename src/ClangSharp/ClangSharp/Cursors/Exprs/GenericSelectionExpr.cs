// Copyright (c) .NET Foundation and Contributors. All Rights Reserved. Licensed under the MIT License (MIT). See License.md in the repository root for more information.

using System;
using System.Collections.Generic;
using System.Linq;
using ClangSharp.Interop;

namespace ClangSharp;

public sealed class GenericSelectionExpr : Expr
{
    private readonly Lazy<IReadOnlyList<Expr>> _assocExprs;

    internal GenericSelectionExpr(CXCursor handle) : base(handle, CXCursorKind.CXCursor_GenericSelectionExpr, CX_StmtClass.CX_StmtClass_GenericSelectionExpr)
    {
        _assocExprs = new Lazy<IReadOnlyList<Expr>>(() => Children.Skip(1).Take((int)NumAssocs).Cast<Expr>().ToList());
    }

    public IReadOnlyList<Expr> AssocExprs => _assocExprs.Value;

    public Expr ControllingExpr => (Expr)Children[0];

    public bool IsResultDependent => Handle.IsResultDependent;

    public uint NumAssocs => unchecked((uint)Handle.NumAssocs);

    public Expr? ResultExpr => IsResultDependent ? null : (Expr)Children[(int)ResultIndex];

    public uint ResultIndex => unchecked((uint)Handle.ResultIndex);
}

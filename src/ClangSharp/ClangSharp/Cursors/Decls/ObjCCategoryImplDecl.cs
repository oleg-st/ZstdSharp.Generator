// Copyright (c) .NET Foundation and Contributors. All Rights Reserved. Licensed under the MIT License (MIT). See License.md in the repository root for more information.

using System;
using ClangSharp.Interop;

namespace ClangSharp;

public sealed class ObjCCategoryImplDecl : ObjCImplDecl
{
    private readonly Lazy<ObjCCategoryDecl> _categoryDecl;

    internal ObjCCategoryImplDecl(CXCursor handle) : base(handle, CXCursorKind.CXCursor_ObjCCategoryImplDecl, CX_DeclKind.CX_DeclKind_ObjCCategoryImpl)
    {
        _categoryDecl = new Lazy<ObjCCategoryDecl>(() => TranslationUnit.GetOrCreate<ObjCCategoryDecl>(Handle.GetSubDecl(1)));
    }

    public ObjCCategoryDecl CategoryDecl => _categoryDecl.Value;
}

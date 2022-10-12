// Copyright (c) .NET Foundation and Contributors. All Rights Reserved. Licensed under the MIT License (MIT). See License.md in the repository root for more information.

using System;
using ClangSharp.Interop;

namespace ClangSharp;

public sealed class TypedefType : Type
{
    private readonly Lazy<TypedefNameDecl> _decl;

    internal TypedefType(CXType handle) : base(handle, CXTypeKind.CXType_Typedef, CX_TypeClass.CX_TypeClass_Typedef)
    {
        _decl = new Lazy<TypedefNameDecl>(() => TranslationUnit.GetOrCreate<TypedefNameDecl>(Handle.Declaration));
    }

    public TypedefNameDecl Decl => _decl.Value;
}

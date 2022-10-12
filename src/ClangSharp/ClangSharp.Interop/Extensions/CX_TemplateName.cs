// Copyright (c) .NET Foundation and Contributors. All Rights Reserved. Licensed under the MIT License (MIT). See License.md in the repository root for more information.
// Ported from https://github.com/dotnet/clangsharp/blob/main/sources/libClangSharp

namespace ClangSharp.Interop;

public unsafe partial struct CX_TemplateName
{
    public CXCursor AsTemplateDecl => clangsharp.TemplateName_getAsTemplateDecl(this);
}

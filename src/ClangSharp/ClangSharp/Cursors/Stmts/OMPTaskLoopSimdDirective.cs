// Copyright (c) .NET Foundation and Contributors. All Rights Reserved. Licensed under the MIT License (MIT). See License.md in the repository root for more information.

using ClangSharp.Interop;

namespace ClangSharp;

public sealed class OMPTaskLoopSimdDirective : OMPLoopDirective
{
    internal OMPTaskLoopSimdDirective(CXCursor handle) : base(handle, CXCursorKind.CXCursor_OMPTaskLoopSimdDirective, CX_StmtClass.CX_StmtClass_OMPTaskLoopSimdDirective)
    {
    }
}

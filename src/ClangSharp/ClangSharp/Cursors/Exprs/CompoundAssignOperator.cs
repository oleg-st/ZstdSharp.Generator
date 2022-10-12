// Copyright (c) .NET Foundation and Contributors. All Rights Reserved. Licensed under the MIT License (MIT). See License.md in the repository root for more information.

using System;
using ClangSharp.Interop;

namespace ClangSharp;

public sealed class CompoundAssignOperator : BinaryOperator
{
    private readonly Lazy<Type> _computationLHSType;
    private readonly Lazy<Type> _computationResultType;

    internal CompoundAssignOperator(CXCursor handle) : base(handle, CXCursorKind.CXCursor_CompoundAssignOperator, CX_StmtClass.CX_StmtClass_CompoundAssignOperator)
    {
        _computationLHSType = new Lazy<Type>(() => TranslationUnit.GetOrCreate<Type>(handle.ComputationLhsType));
        _computationResultType = new Lazy<Type>(() => TranslationUnit.GetOrCreate<Type>(handle.ComputationResultType));
    }

    public Type ComputationLHSType => _computationLHSType.Value;

    public Type ComputationResultType => _computationResultType.Value;
}

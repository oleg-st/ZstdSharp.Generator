using System;
using System.Collections.Generic;
using System.Linq;
using ClangSharp;
using ClangSharp.Interop;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ZstdSharp.Generator.CodeGenerator.Extensions;
using Type = ClangSharp.Type;
// ReSharper disable StringLiteralTypo
// ReSharper disable CommentTypo

namespace ZstdSharp.Generator.CodeGenerator.TypeCaster;

internal class TypeCaster
{
    private readonly CodeGenerator _codeGenerator;
    private readonly CallReplacer _callReplacer;

    public TypeCaster(CodeGenerator codeGenerator, CallReplacer callReplacer)
    {
        _codeGenerator = codeGenerator;
        _callReplacer = callReplacer;
    }

    public class Casts : List<CustomCast>
    {
    }

    private readonly Dictionary<Expr, Casts> _casts = new();

    public class CustomCast
    {
        public CustomType Source { get; }

        public CustomType Target { get; }

        public CustomCast(CustomType source, CustomType target)
        {
            Source = source;
            Target = target;
        }

        public SyntaxNode Apply(Expr expr, CodeGenerator codeGenerator, ExpressionSyntax node)
        {
            if (Target is LogicalBinaryType logicalBinaryType && node is BinaryExpressionSyntax binaryExpressionSyntax)
            {
                return SyntaxFactory.BinaryExpression(logicalBinaryType.SyntaxKind, binaryExpressionSyntax.Left,
                    binaryExpressionSyntax.Right);
            }

            if (Target is NegateBoolType)
            {
                // remove !
                if (node is PrefixUnaryExpressionSyntax prefixUnaryExpressionSyntax &&
                    prefixUnaryExpressionSyntax.Kind() == SyntaxKind.LogicalNotExpression)
                {
                    node = prefixUnaryExpressionSyntax.Operand;
                }

                // ==
                return SyntaxFactory.ParenthesizedExpression(SyntaxFactory.BinaryExpression(
                    SyntaxKind.EqualsExpression,
                    SyntaxFactory.ParenthesizedExpression(node), Source switch
                    {
                        // 0
                        IntegerType =>
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.NumericLiteralExpression,
                                SyntaxFactory.Literal(0)),
                        // null
                        PointerType => SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression),
                        // default
                        _ => SyntaxFactory.LiteralExpression(SyntaxKind.DefaultLiteralExpression)
                    }));
            }

            if (Target is BoolType)
            {
                // !=
                return SyntaxFactory.ParenthesizedExpression(SyntaxFactory.BinaryExpression(
                    SyntaxKind.NotEqualsExpression,
                    SyntaxFactory.ParenthesizedExpression(node), Source switch
                    {
                        // 0
                        IntegerType =>
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.NumericLiteralExpression,
                                SyntaxFactory.Literal(0)),
                        // null
                        PointerType => SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression),
                        // default
                        _ => SyntaxFactory.LiteralExpression(SyntaxKind.DefaultLiteralExpression)
                    }));
            }

            if (Source is BoolType && Target is IntegerType ti)
            {
                // less than int : short / byte
                if (ti.Size < 4)
                {
                    // (type)(node ? 1 : 0)
                    return codeGenerator.CreateCast(
                        expr,
                        codeGenerator.GetType(Target.Name),
                            SyntaxFactory.ConditionalExpression(
                                node,
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    SyntaxFactory.Literal(1)),
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    SyntaxFactory.Literal(0))));
                }

                // (node ? 1 : 0) / (node ? 1U : 0U)
                return SyntaxFactory.ParenthesizedExpression(
                    SyntaxFactory.ConditionalExpression(
                        SyntaxFactory.ParenthesizedExpression(node),
                        ti.IsSigned
                            ? SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                SyntaxFactory.Literal(1))
                            : SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                SyntaxFactory.Literal(1U)),
                        ti.IsSigned
                            ? SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                SyntaxFactory.Literal(0))
                            : SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                                SyntaxFactory.Literal(0U))));
            }

            if (Source is BoolType && Target is EnumType)
            {
                // (type)(node ? 1 : 0)
                return codeGenerator.CreateCast(
                    expr,
                    codeGenerator.GetType(Target.Name),
                        SyntaxFactory.ConditionalExpression(
                            node,
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.NumericLiteralExpression,
                                SyntaxFactory.Literal(1)),
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.NumericLiteralExpression,
                                SyntaxFactory.Literal(0))));
            }

            if (CheckIsUnchecked(Source, Target))
            {
                // unchecked((type)node)
                return SyntaxFactory.CheckedExpression(SyntaxKind.UncheckedExpression,
                    codeGenerator.CreateCast(expr, codeGenerator.GetType(Target.Name), node));
            }

            // null to pointer
            if (Target is PointerType && node is LiteralExpressionSyntax literalExpressionSyntax &&
                literalExpressionSyntax.Kind() == SyntaxKind.NullLiteralExpression)
            {
                return node;
            }

            // function -> (delegate *...)&function, &function -> (delegate*)&function
            if (codeGenerator.UseFunctionPointerForType(Target.Name) && Target is FunctionPointerType)
            {
                PrefixUnaryExpressionSyntax? addrOfNode = null;
                if (node is PrefixUnaryExpressionSyntax prefixUnaryExpressionSyntax &&
                    prefixUnaryExpressionSyntax.Kind() == SyntaxKind.AddressOfExpression)
                {
                    addrOfNode = prefixUnaryExpressionSyntax;
                } else if (node is IdentifierNameSyntax)
                {
                    addrOfNode = SyntaxFactory.PrefixUnaryExpression(SyntaxKind.AddressOfExpression, node);
                }

                if (addrOfNode != null)
                {
                    if (codeGenerator.Config.HideFunctionPointers)
                    {
                        var functionProtoType = codeGenerator.GetFunctionProtoType(expr);
                        if (functionProtoType != null)
                        {
                            var functionPointerType = codeGenerator.GetFunctionPointerType(expr, functionProtoType);
                            return codeGenerator.CreateCast(expr, functionPointerType, addrOfNode);
                        }
                    }
                    return addrOfNode;
                }
            }

            // function to IntPtr -> function
            if (Target.Name == "IntPtr" && Target is FunctionPointerType && node is IdentifierNameSyntax)
            {
                return node;
            }

            // (T*)struct -> (T*)(&struct)
            var innerExpr = codeGenerator.GetInnerExpr(expr);
            if (innerExpr is ImplicitCastExpr {CastKind: CX_CastKind.CX_CK_ArrayToPointerDecay} implicitCastExpr &&
                codeGenerator.IsSupportedFixedBufferType(implicitCastExpr.SubExpr))
            {
                node = SyntaxFactory.ParenthesizedExpression(SyntaxFactory.PrefixUnaryExpression(SyntaxKind.AddressOfExpression, node));
            }

            // (type)node
            return codeGenerator.CreateCast(expr, codeGenerator.GetType(Target.Name), node);
        }
    }

    private static bool CheckIsUnchecked(CustomType source, CustomType target)
    {
        return source is IntegerType sourceInteger &&
               target is IntegerType {IsSigned: false} &&
               sourceInteger.Value is < 0;
    }

    public class CustomType
    {
        public string Name { get; protected init; }

        protected CustomType(string name)
        {
            Name = name;
        }

        public virtual CustomType Inherit()
        {
            return this;
        }
    }

    public class BoolType : CustomType
    {
        public BoolType() : base("bool")
        {
        }
    }

    public class StringType : CustomType
    {
        public StringType() : base("string")
        {
        }
    }

    public class NegateBoolType : BoolType
    {
    }

    public class LogicalBinaryType : BoolType
    {
        public SyntaxKind SyntaxKind { get; }
        public LogicalBinaryType(SyntaxKind syntaxKind)
        {
            SyntaxKind = syntaxKind;
        }
    }

    public class VoidType : CustomType
    {
        public VoidType() : base("void")
        {
        }
    }

    public class OtherType : CustomType
    {
        public OtherType() : base("")
        {
        }
    }

    public class EnumType : CustomType
    {
        public EnumType(string name) : base(name)
        {
        }
    }

    public class RecordType : CustomType
    {
        public RecordType(string name) : base(name)
        {
        }
    }

    public class PointerType : CustomType
    {
        public PointerType(string name) : base(name)
        {
        }
    }

    public class FunctionPointerType : PointerType
    {
        public FunctionPointerType(string name) : base(name)
        {
        }
    }

    public class Vector128Type : CustomType
    {
        public Vector128Type(string name) : base(name)
        {
        }
    }

    public class IntegerType : CustomType
    {
        public int Size { get; }
        public bool IsSigned { get; }

        public long? Value { get; private init; }

        public bool IsConstant { get; private init; }

        public bool HasValue => Value.HasValue;

        // between 4 and 8
        private const int NativeSize = 999;

        public bool IsNative => Size == NativeSize;

        public static int MaxSize(int a, int b)
        {
            // 1 -> 4 -> n -> 8
            if (a >= 8 && b == NativeSize)
            {
                return a;
            }

            if (b >= 8 && a == NativeSize)
            {
                return b;
            }

            return Math.Max(a, b);
        }

        public IntegerType(int size, bool isSigned) : base(size switch
        {
            NativeSize => isSigned ? "nint" : "nuint",
            1 => isSigned ? "sbyte" : "byte",
            2 => isSigned ? "short" : "ushort",
            4 => isSigned ? "int" : "uint",
            8 => isSigned ? "long" : "ulong",
            _ => throw new Exception($"Unknown integer type {size}")
        })
        {
            Size = size;
            IsSigned = isSigned;
        }

        public static IntegerType Create(string name)
        {
            return name switch
            {
                "nuint" => new IntegerType(NativeSize, false),
                "nint" => new IntegerType(NativeSize, true),
                "byte" => new IntegerType(1, false),
                "sbyte" => new IntegerType(1, true),
                "ushort" => new IntegerType(2, false),
                "short" => new IntegerType(2, true),
                "uint" => new IntegerType(4, false),
                "int" => new IntegerType(4, true),
                "ulong" => new IntegerType(8, false),
                "long" => new IntegerType(8, true),
                _ => throw new Exception($"Unknown integer type {name}")
            };
        }

        public override CustomType Inherit()
        {
            return new IntegerType(Size, IsSigned);
        }

        public CustomType InheritWithValue(long value)
        {
            return new IntegerType(Size, IsSigned) {Value = value, IsConstant = true};
        }

        public CustomType InheritWithConstant()
        {
            return new IntegerType(Size, IsSigned) {IsConstant = true};
        }
    }

    private CustomType CastTo(Expr expr, CustomType source, CustomType target, bool strong = false)
    {
        if (target is not OtherType && !IsCompatible(source, target, strong))
        {
            if (!_casts.TryGetValue(expr, out var casts))
            {
                _casts[expr] = casts = new Casts();
            }
            casts.Add(new CustomCast(source, target));
        }

        return target;
    }

    private CustomType GetExprType(Expr expr)
    {
        if (_casts.TryGetValue(expr, out var casts))
        {
            return casts.Last().Target;
        }

        if (expr is ParenExpr parenExpr)
        {
            return GetExprType(parenExpr.SubExpr);
        }

        if (expr is ImplicitCastExpr implicitCastExpr)
        {
            return GetExprType(implicitCastExpr.SubExpr);
        }

        if (expr is UnaryExprOrTypeTraitExpr {Kind: CX_UnaryExprOrTypeTrait.CX_UETT_SizeOf} unaryExprOrTypeTraitExpr)
        {
            if (_codeGenerator.IsSizeOfConst(unaryExprOrTypeTraitExpr) && IsPrevContextDecl<VarDecl>(out _))
            {
                return IntegerType.Create("int").InheritWithConstant();
            }

            return IntegerType.Create("int");
        }

        if (expr is UnaryExprOrTypeTraitExpr {Kind: CX_UnaryExprOrTypeTrait.CX_UETT_AlignOf or CX_UnaryExprOrTypeTrait.CX_UETT_PreferredAlignOf})
        {
            return IntegerType.Create("int");
        }

        if (expr is CStyleCastExpr cStyleCastExpr)
        {
            var l = GetExprType(cStyleCastExpr.SubExpr);
            var r = GetCustomType(expr, expr.Type);

            // bool to int before enum
            if (l is BoolType && r is EnumType)
            {
                CastTo(cStyleCastExpr.SubExpr, l, IntegerType.Create("int"));
            }

            // bool to int
            if (l is BoolType && r is IntegerType)
            {
                CastTo(cStyleCastExpr.SubExpr, l, r);
            }

            return r;
        }

        if (expr is DeclRefExpr {Decl: EnumConstantDecl} declRefExpr)
        {
            if (declRefExpr.Decl is EnumConstantDecl {DeclContext: EnumDecl enumDecl})
            {
                // todo
                return new EnumType(_codeGenerator.GetRemappedCSharpType(enumDecl, enumDecl.TypeForDecl, out _).ToString());
            }
        }

        if (expr is StringLiteral)
        {
            return new StringType();
        }

        if (expr is BinaryOperator binaryOperator)
        {
            if (binaryOperator.Opcode is CXBinaryOperatorKind.CXBinaryOperator_LOr or CXBinaryOperatorKind.CXBinaryOperator_LAnd)
            {
                // bool [&&] bool => bool

                var l = GetExprType(binaryOperator.LHS);
                var r = GetExprType(binaryOperator.RHS);

                if (l is EnumType or IntegerType or PointerType)
                {
                    CastTo(binaryOperator.LHS, l, new BoolType());
                }

                if (r is EnumType or IntegerType or PointerType)
                {
                    CastTo(binaryOperator.RHS, r, new BoolType());
                }

                return new BoolType();
            }

            if (binaryOperator.Opcode is CXBinaryOperatorKind.CXBinaryOperator_LE or CXBinaryOperatorKind.CXBinaryOperator_GE
                or CXBinaryOperatorKind.CXBinaryOperator_EQ or CXBinaryOperatorKind.CXBinaryOperator_NE or CXBinaryOperatorKind.CXBinaryOperator_LT
                or CXBinaryOperatorKind.CXBinaryOperator_GT)
            {
                // same [==] same => bool

                var l = GetExprType(binaryOperator.LHS);
                var r = GetExprType(binaryOperator.RHS);

                if (l is IntegerType && r is EnumType)
                {
                    // enum to int
                    CastTo(binaryOperator.RHS, r, l);
                }

                if (l is EnumType && r is IntegerType)
                {
                    // enum to int
                    CastTo(binaryOperator.LHS, l, r);
                }

                if (l is IntegerType li && r is IntegerType ri)
                {
                    MatchIntegers(li, ri, binaryOperator.LHS, binaryOperator.RHS, l, r);
                }

                return new BoolType();
            }

            if (
                binaryOperator.Opcode is CXBinaryOperatorKind.CXBinaryOperator_AddAssign or CXBinaryOperatorKind.CXBinaryOperator_SubAssign
                or CXBinaryOperatorKind.CXBinaryOperator_XorAssign or CXBinaryOperatorKind.CXBinaryOperator_OrAssign
                or CXBinaryOperatorKind.CXBinaryOperator_AndAssign or CXBinaryOperatorKind.CXBinaryOperator_Assign
                or CXBinaryOperatorKind.CXBinaryOperator_MulAssign or CXBinaryOperatorKind.CXBinaryOperator_DivAssign
            )
            {
                // same [+=] same => same

                var l = GetExprType(binaryOperator.LHS);
                var r = GetExprType(binaryOperator.RHS);

                if (l is IntegerType lbi && r is BoolType)
                {
                    CastTo(binaryOperator.RHS, r, lbi, true);
                }

                if (l is PointerType && r is BoolType)
                {
                    CastTo(binaryOperator.RHS, r, IntegerType.Create("int"), true);
                }

                if (l is IntegerType && r is IntegerType)
                {
                    CastTo(binaryOperator.RHS, r, l, true);
                }

                if (l is IntegerType && r is EnumType)
                {
                    CastTo(binaryOperator.RHS, r, l, true);
                }

                if (l is EnumType && r is BoolType)
                {
                    CastTo(binaryOperator.RHS, r, l, true);
                }

                return l.Inherit();
            }

            if (binaryOperator.Opcode is CXBinaryOperatorKind.CXBinaryOperator_Add or CXBinaryOperatorKind.CXBinaryOperator_Sub
                or CXBinaryOperatorKind.CXBinaryOperator_Xor or CXBinaryOperatorKind.CXBinaryOperator_Or or CXBinaryOperatorKind.CXBinaryOperator_And
                or CXBinaryOperatorKind.CXBinaryOperator_Mul or CXBinaryOperatorKind.CXBinaryOperator_Div or CXBinaryOperatorKind.CXBinaryOperator_Rem
               )
            {
                // same [+] same => same

                var l = GetExprType(binaryOperator.LHS);
                var r = GetExprType(binaryOperator.RHS);

                // bool | bool, bool & bool
                if ((binaryOperator.Opcode == CXBinaryOperatorKind.CXBinaryOperator_And ||
                     binaryOperator.Opcode == CXBinaryOperatorKind.CXBinaryOperator_Or) && l is BoolType && r is BoolType)
                {
                    var kind = binaryOperator.Opcode == CXBinaryOperatorKind.CXBinaryOperator_And
                        ? SyntaxKind.LogicalAndExpression
                        : SyntaxKind.LogicalOrExpression;
                    return CastTo(expr, new OtherType(), new LogicalBinaryType(kind));
                }

                if (binaryOperator.Opcode == CXBinaryOperatorKind.CXBinaryOperator_Sub && l is PointerType && r is PointerType)
                {
                    // ptr diff is long
                    return IntegerType.Create("long");
                }

                if (l is BoolType)
                {
                    var targetType = IntegerType.Create("int");
                    CastTo(binaryOperator.LHS, l, targetType);
                    l = targetType;
                }

                if (r is BoolType)
                {
                    var targetType = IntegerType.Create("int");
                    CastTo(binaryOperator.RHS, r, targetType);
                    r = targetType;
                }

                if (l is PointerType && r is IntegerType)
                {
                    return l.Inherit();
                }

                if (l is IntegerType && r is PointerType)
                {
                    return r.Inherit();
                }

                if (l is IntegerType li && r is IntegerType ri)
                {
                    return MatchIntegers(li, ri, binaryOperator.LHS, binaryOperator.RHS, l, r);
                }

                // enum [+-] int -> pass
                // enum [op] integer -> (integer)enum [op] integer
                if (l is EnumType && r is IntegerType && !(r.Name == "int" && binaryOperator.Opcode is CXBinaryOperatorKind.CXBinaryOperator_Add or CXBinaryOperatorKind.CXBinaryOperator_Sub))
                {
                    CastTo(binaryOperator.LHS, l, r, true);
                    return r.Inherit();
                }

                // int [+-] enum -> pass
                // // integer [op] enum -> (integer)enum [op] integer
                if (!(l.Name == "int" && binaryOperator.Opcode is CXBinaryOperatorKind.CXBinaryOperator_Add or CXBinaryOperatorKind.CXBinaryOperator_Sub) && r is EnumType)
                {
                    CastTo(binaryOperator.RHS, r, l, true);
                    return l.Inherit();
                }
            }

            if (binaryOperator.Opcode is CXBinaryOperatorKind.CXBinaryOperator_Shl or CXBinaryOperatorKind.CXBinaryOperator_ShlAssign or CXBinaryOperatorKind.CXBinaryOperator_Shr or CXBinaryOperatorKind.CXBinaryOperator_ShrAssign)
            {
                // same [>>] int => same

                var l = GetExprType(binaryOperator.LHS);
                var r = GetExprType(binaryOperator.RHS);

                if (l is BoolType)
                {
                    l = CastTo(binaryOperator.LHS, l, IntegerType.Create("int"));
                }

                if (l is IntegerType && r is IntegerType ri)
                {
                    // (int)
                    if (ri.Name != "int")
                    {
                        CastTo(binaryOperator.RHS, r, IntegerType.Create("int"));
                    }
                }

                // byte << int => int
                if (l is IntegerType {Size: < 4})
                {
                    return IntegerType.Create("int");
                }

                return l.Inherit();
            }
        }

        if (expr is UnaryOperator unaryOperator)
        {
            if (unaryOperator.Opcode == CXUnaryOperatorKind.CXUnaryOperator_LNot)
            {
                var l = GetExprType(unaryOperator.SubExpr);

                if (l is IntegerType or EnumType or PointerType)
                {
                    CastTo(expr, l, new NegateBoolType());
                }

                return new BoolType();
            }

            if (unaryOperator.Opcode == CXUnaryOperatorKind.CXUnaryOperator_Minus)
            {
                var l = GetExprType(unaryOperator.SubExpr);

                if (l is EnumType or BoolType)
                {
                    var targetType = IntegerType.Create("int");
                    CastTo(unaryOperator.SubExpr, l, targetType);
                    return targetType;
                }
            }

            if (unaryOperator.Opcode == CXUnaryOperatorKind.CXUnaryOperator_Not)
            {
                var l = GetExprType(unaryOperator.SubExpr);
                if (l is IntegerType {HasValue: true, Value: { }} li)
                {
                    return li.InheritWithValue(~li.Value.Value);
                }
            }

            if (unaryOperator.Opcode == CXUnaryOperatorKind.CXUnaryOperator_AddrOf)
            {
                // &function -> function
                if (unaryOperator.SubExpr.Type is FunctionProtoType)
                {
                    return GetExprType(unaryOperator.SubExpr);
                }
            }
        }

        if (expr is ConditionalOperator conditionalOperator)
        {
            var c = GetExprType(conditionalOperator.Cond);

            if (c is EnumType or IntegerType or PointerType)
            {
                CastTo(conditionalOperator.Cond, c, new BoolType());
            }

            var l = GetExprType(conditionalOperator.LHS);
            var r = GetExprType(conditionalOperator.RHS);

            // todo FIXME
            var lt = GetCustomType(conditionalOperator.LHS, conditionalOperator.LHS.Type);
            var rt = GetCustomType(conditionalOperator.RHS, conditionalOperator.RHS.Type);

            if (lt is FunctionPointerType)
            {
                CastTo(conditionalOperator.LHS, l, lt);
            }

            if (rt is FunctionPointerType)
            {
                CastTo(conditionalOperator.RHS, r, rt);
            }

            if (l is BoolType && r is IntegerType)
            {
                CastTo(conditionalOperator.LHS, l, r);
            }

            if (r is BoolType && l is IntegerType)
            {
                CastTo(conditionalOperator.RHS, r, l);
            }

            if (l is EnumType && r is IntegerType)
            {
                CastTo(conditionalOperator.LHS, l, r);
            }

            if (r is EnumType && l is IntegerType)
            {
                CastTo(conditionalOperator.RHS, r, l);
            }

            if (l is IntegerType li && r is IntegerType ri)
            {
                return MatchIntegers(li, ri, conditionalOperator.LHS, conditionalOperator.RHS, l, r);
            }

            if (IsCompatible(l, r, false))
            {
                return l.Inherit();
            }
        }

        if (expr is ArraySubscriptExpr arraySubscriptExpr)
        {
            (_, Expr idxExpr) = _codeGenerator.ExtractArraySubscriptExpr(arraySubscriptExpr);

            var r = GetExprType(idxExpr);

            if (r is BoolType or EnumType)
            {
                CastTo(idxExpr, r, IntegerType.Create("int"));
            }
        }

        if (expr is CallExpr callExpr)
        {
            var callReplacement = _callReplacer.GetCallReplacement(callExpr);
            if (callReplacement != null)
            {
                return callReplacement.Type;
            }
        }

        return GetCustomType(expr, expr.Type);
    }

    private FunctionProtoType? GetFunctionProtoType(CallExpr callExpr)
    {
        return _codeGenerator.UnwrapElaborated(callExpr.Callee.Type) switch
        {
            ClangSharp.PointerType {PointeeType: FunctionProtoType directFunctionProtoType} =>
                directFunctionProtoType,
            TypedefType {PointeeType: FunctionProtoType functionProtoType} => functionProtoType,
            _ => null
        };
    }

    private CustomType MatchIntegers(IntegerType li, IntegerType ri, Expr lhs, Expr rhs, CustomType l, CustomType r)
    {
        if ((li.Size < 4 && ri.Size < 4) || (li.Name == "int" && ri.Size < 4) || (ri.Name == "int" && li.Size < 4))
        {
            return IntegerType.Create("int");
        }

        if (li.IsSigned && !ri.IsSigned && !li.IsConstant)
        {
            // int & nuint -> (uint)int & nuint -> nuint
            // long & nuint -> (nuint)long & nuint -> ulong
            return CastTo(lhs, l, ri);
        }

        if (!li.IsSigned && ri.IsSigned && !ri.IsConstant)
        {
            // nuint & int -> nuint & (uint)int -> nuint
            return CastTo(rhs, r, li);
        }

        return new IntegerType(IntegerType.MaxSize(li.Size, ri.Size), li.IsSigned && ri.IsSigned);
    }

    private CustomType GetCustomType(Cursor cursor, Type type)
    {
        var cSharpType = _codeGenerator.GetRemappedCSharpType(cursor, type, out _);
        // todo
        var typeName = cSharpType.ToString();
        if (type.CanonicalType.IsIntegerType)
        {
            if (type.CanonicalType.Kind == CXTypeKind.CXType_Bool)
            {
                return new BoolType();
            }

            var integerType = IntegerType.Create(typeName);
            if (cursor is IntegerLiteral integerLiteral)
            {
                return integerType.InheritWithValue(integerLiteral.Value);
            }

            return integerType;
        }

        if (type.CanonicalType.IsPointerType)
        {
            if (type.CanonicalType is ClangSharp.PointerType pointerType && pointerType.CanonicalType.PointeeType is FunctionProtoType)
            {
                return new FunctionPointerType(typeName);
            }

            return new PointerType(typeName);
        }

        if (type.CanonicalType is ClangSharp.EnumType)
        {
            return new EnumType(typeName);
        }

        if (type.CanonicalType is ClangSharp.RecordType)
        {
            return new RecordType(typeName);
        }

        return new OtherType();
    }

    public Casts? GetCasts(Expr expr)
    {
        return _casts.TryGetValue(expr, out var cast) ? cast : null;
    }

    private static bool IsCompatibleValue(IntegerType li, IntegerType ri)
    {
        if (ri.Value != null)
        {
            return !CheckIsUnchecked(ri, li);
        }

        if (li.Value != null)
        {
            return !CheckIsUnchecked(li, ri);
        }

        return false;
    }

    private bool IsCompatible(CustomType l, CustomType r, bool strong)
    {
        switch (l)
        {
            case BoolType when r is BoolType:
                return true;
            case IntegerType li when r is IntegerType ri:
            {
                if (li.IsNative && ri.Size == 8)
                {
                    return false;
                }

                if (li.Size == 8 && ri.IsNative)
                {
                    return false;
                }

                if ((li.IsSigned == ri.IsSigned && (!strong || li.Size <= ri.Size)) || IsCompatibleValue(li, ri))
                {
                    return true;
                }

                break;
            }
        }

        // any pointer is compatible with void*
        if (l is PointerType && r is PointerType {Name: "void*"})
        {
            return true;
        }

        // (sbyte*)"" -> ""
        if (l is StringType && r is PointerType {Name: "sbyte*"})
        {
            return true;
        }

        return !string.IsNullOrEmpty(l.Name) &&
               !string.IsNullOrEmpty(r.Name) &&
               l.Name == r.Name;
    }

    private bool IsPrevContextDecl<T>(out T? value)
        where T : Decl
    {
        var firstDecl = _codeGenerator.ReverseContext().OfType<Decl>().FirstOrDefault();
        if (firstDecl is T decl)
        {
            value = decl;
            return true;
        }

        value = null;
        return false;
    }

    public void Visit(Expr expr)
    {
        // already processed
        if (_casts.ContainsKey(expr))
        {
            return;
        }

        var l = GetExprType(expr);
        if (_codeGenerator.IsPrevBoolContext(expr))
        {
            // to bool
            if (l is EnumType or IntegerType or PointerType)
            {
                CastTo(expr, l, new BoolType());
            }
        } else if (_codeGenerator.GetPrevContext<ReturnStmt>(out var returnStmt) && returnStmt.RetValue == expr)
        {
            // return to function return type
            if (IsPrevContextDecl<FunctionDecl>(out var functionDecl))
            {
                var callReplacement = _callReplacer.GetCallReplacement(functionDecl!.Name);
                var retType = callReplacement != null ? callReplacement.Type : GetCustomType(functionDecl, functionDecl.ReturnType);
                CastTo(expr, l, retType, true);
            }
        } else if (_codeGenerator.GetPrevContext<CallExpr>(out var callExpr))
        {
            // call's arguments
            var index = callExpr.Args.IndexOf(expr);
            if (index >= 0)
            {
                var callReplacement = _callReplacer.GetCallReplacement(callExpr);
                if (callReplacement is {WithArguments: true})
                {
                    var argType = callReplacement.GetArgumentType(index);
                    if (argType != null)
                    {
                        CastTo(expr, l, argType, true);
                    }
                }
                else
                {
                    var functionProtoType = GetFunctionProtoType(callExpr);
                    if (functionProtoType != null)
                    {
                        if (index < functionProtoType.ParamTypes.Count)
                        {
                            var argType = GetCustomType(expr, functionProtoType.ParamTypes[index]);
                            CastTo(expr, l, argType, true);
                        }
                    }
                }
            }
        } else if (_codeGenerator.GetPrevContext<VarDecl>(out var varDecl))
        {
            // var init to var type
            var varType = GetCustomType(varDecl, varDecl.Type);
            CastTo(expr, l, varType, true);
        } else if (_codeGenerator.GetPrevContext<InitListExpr>(out var initListExpr))
        {
            var type = GetInnerType(initListExpr.Type);
            if (type is ClangSharp.RecordType recordType)
            {
                // record init to field type
                var index = initListExpr.Inits.IndexOf(expr);
                if (index >= 0)
                {
                    var varType = GetCustomType(expr, recordType.Decl.Fields[index].Type);
                    CastTo(expr, l, varType, true);
                }
            }
            else if (type is ArrayType arrayType)
            {
                // array init to array element type
                var elementType = GetCustomType(expr, arrayType.ElementType);
                CastTo(expr, l, elementType, true);
            }
        }
    }

    private Type GetInnerType(Type type)
    {
        type = _codeGenerator.UnwrapElaborated(type);
        if (type is TypedefType typedefType)
        {
            return GetInnerType(typedefType.Decl.UnderlyingType);
        }

        return type;
    }
}

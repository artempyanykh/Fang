module Fang.Lang

type VarName = string

module VarName =
    let make (name: string) = name

[<RequireQualifiedAccess>]
type BType =
    | Int of int
    | Unit

type ArithmeticFn =
    | Add
    | Sub
    | Mul
    | Div

type UnaryArithmeticFn = | Neg

type ComparisonFn =
    | Less
    | Equal
    | Greater

type BuiltinFn =
    | Arithmetic of fn: ArithmeticFn * opA: Expr * opB: Expr
    | UnaryArithmetic of fn: UnaryArithmeticFn * op: Expr
    | Comparison of fn: ComparisonFn * lhs: Expr * rhs: Expr

and Expr =
    | Lit of BType
    | Var of VarName
    | Abs of var: VarName * body: Expr
    | App of expr: Expr * arg: Expr
    | Cond of pred: Expr * trueBranch: Expr * falseBranch: Expr
    | Bind of recursive: bool * var: VarName * body: Expr * expr: Expr
    | Builtin of BuiltinFn

//module Expr =
//    let rec containsError =
//        function
//        | Abs (_, body) -> containsError body
//        | App (expr, arg) -> containsError expr || containsError arg
//        | Cond (pred, t, f) ->
//            containsError pred
//            || containsError t
//            || containsError f
//        | Bind (_, _, body, expr) -> containsError body || containsError expr
//        | Builtin (Arithmetic (_, opA, opB)) -> containsError opA || containsError opB
//        | Builtin (UnaryArithmetic (_, expr)) -> containsError expr
//        | Builtin (Comparison (_, expr, rhs)) -> containsError expr || containsError rhs
//        | Error -> true
//        | _ -> false

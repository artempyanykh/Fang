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

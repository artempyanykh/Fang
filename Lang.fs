module Fang.Lang

type VarName = VarName of string

[<RequireQualifiedAccess>]
type BType =
    | Int of int
    | Unit of unit

type ArithmeticFn =
    | Add
    | Sub
    | Mul
    | Div

type ComparisonFn =
    | Less
    | Equal
    | Greater

type BuiltinFn =
    | Arithmetic of fn: ArithmeticFn * opA: Expr * opB: Expr
    | Comparison of fn: ComparisonFn * lhs: Expr * rhs: Expr

and Context = Map<VarName, Expr>

and Expr =
    | Literal of BType
    | Var of VarName
    | Lam of var: VarName * body: Expr
    | App of expr: Expr * arg: Expr
    | Cond of pred: Expr * trueBranch: Expr * falseBranch: Expr
    | Bind of recursive: bool * var: VarName * body: Expr * expr: Expr
    | Builtin of BuiltinFn


let tryExprAsInt: Expr -> Option<int> =
    function
    | Literal (BType.Int value) -> Some value
    | _ -> None

let tryExtractLambdaFromExpr: Expr -> Option<VarName * Expr> =
    function
    | Lam (var, body) -> Some(var, body)
    | _ -> None

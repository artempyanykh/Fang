module Fang.Lang

type VarName = string

module VarName =
    let make (name: string) = name

[<RequireQualifiedAccess>]
type BType =
    | Int of int
    | Unit

type BArithFn =
    | Add
    | Sub
    | Mul
    | Div

type UArithFn = | Neg

type CmpFn =
    | Lt
    | Eq
    | Gt

type PrimFn =
    | BArith of fn: BArithFn * opA: Expr * opB: Expr
    | UArith of fn: UArithFn * op: Expr
    | Cmp of fn: CmpFn * lhs: Expr * rhs: Expr

and Expr =
    | Lit of BType
    | Var of VarName
    | Lam of var: VarName * body: Expr
    | Bind of isRec: bool * var: VarName * body: Expr * expr: Expr
    | App of expr: Expr * arg: Expr
    | Cond of pred: Expr * trueBranch: Expr * falseBranch: Expr
    | Prim of PrimFn

module Expr =
    let rec freeVars =
        function
        | Var name -> Set.singleton name
        | Lam (var, body) -> Set.remove var (freeVars body)
        | App (expr, arg) -> Set.union (freeVars expr) (freeVars arg)
        | Bind (isRec, var, body, cont) ->
            let bodyVars = freeVars body

            let bodyVars =
                if isRec then
                    Set.remove var bodyVars
                else
                    bodyVars

            let contVars = freeVars cont
            let contVars = Set.remove var contVars

            Set.union bodyVars contVars
        | Cond (p, t, f) ->
            Set.unionMany [ freeVars p
                            freeVars t
                            freeVars f ]
        | Prim (BArith (_, opA, opB)) -> Set.union (freeVars opA) (freeVars opB)
        | Prim (UArith (_, opA)) -> freeVars opA
        | Prim (Cmp (_, lhs, rhs)) -> Set.union (freeVars lhs) (freeVars rhs)
        | _ -> Set.empty

    let collapseLambdas (expr: Expr) : List<VarName> * Expr =
        let rec go acc =
            function
            | Lam (var, body) -> go (var :: acc) body
            | other -> (acc, other)

        let varsRev, body = go [] expr
        List.rev varsRev, body

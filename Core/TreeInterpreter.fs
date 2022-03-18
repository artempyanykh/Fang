module Fang.TreeInterpreter

open Fang.Lang

type Env = Map<VarName, Value>

and Closure =
    { mutable env: Env
      var: VarName
      body: Expr }


and [<Struct>] Value =
    | Int of intVal: int
    | Closure of closureVal: Closure
    | BlackHole

module Env =
    let empty = Map.empty

    let inline isEmpty e = Map.isEmpty e

    let inline lookupVar (var: VarName) (e: Env) = Map.tryFind var e

    let inline add var closure (e: Env) = Map.add var closure e

type EvalError =
    | UnboundName of VarName
    | WrongValue of Value * expectedKind: string
    | InfiniteRecBinding of VarName

exception EvalException of EvalError

let evalArithmetic fn a b =
    match fn with
    | Add -> a + b
    | Sub -> a - b
    | Mul -> a * b
    | Div -> a / b

let evalComparison fn l r =
    match fn with
    | Lt -> if l < r then 1 else 0
    | Gt -> if l > r then 1 else 0
    | Eq -> if l = r then 1 else 0

let valueAsInt =
    function
    | Int expr -> expr
    | other -> raise (EvalException(WrongValue(other, "int")))

let valueAsClosure =
    function
    | Closure c -> c
    | other -> raise (EvalException(WrongValue(other, "closure")))

let checkBlackHole var =
    function
    | BlackHole -> raise (EvalException(InfiniteRecBinding var))
    | other -> other

let rec eval (env: Env) (expr: Expr) : Value =
    match expr with
    | Lit BType.Unit -> Value.Int 0
    | Lit (BType.Int i) -> Value.Int i
    | Var var ->
        Env.lookupVar var env
        |> Option.map (checkBlackHole var)
        |> Option.defaultWith (fun () -> raise (EvalException(UnboundName var)))
    | Lam (var, body) -> Closure { env = env; var = var; body = body }
    | Prim (BArith (fn, a, b)) ->
        let a = eval env a |> valueAsInt
        let b = eval env b |> valueAsInt

        Value.Int(evalArithmetic fn a b)
    | Prim (UArith (fn, expr)) ->
        let exprVal = eval env expr |> valueAsInt

        match fn with
        | UArithFn.Neg -> Value.Int(-exprVal)
    | Prim (Cmp (fn, l, r)) ->
        let a = eval env l |> valueAsInt
        let b = eval env r |> valueAsInt

        Value.Int(evalComparison fn a b)
    | Bind (recursive, var, body, expr) ->
        let bodyEnv =
            if recursive then
                Env.add var Value.BlackHole env
            else
                env

        let bodyVal =
            match eval bodyEnv body with
            | Closure c as v ->
                let closedEnv = Env.add var v env
                c.env <- closedEnv
                v
            | other -> other


        let exprEnv = Env.add var bodyVal env

        eval exprEnv expr
    | App (expr, arg) ->
        let arg = eval env arg
        let closure = eval env expr |> valueAsClosure
        let appEnv = Env.add closure.var arg closure.env

        eval appEnv closure.body
    | Cond (pred, t, f) ->
        let pred = eval env pred |> valueAsInt

        if pred <> 0 then
            eval env t
        else
            eval env f

module Ex =
    let evalExpr = eval Env.empty
    
    let evalPrint expr =
        let start = System.DateTime.Now

        try
            let expr = eval Env.empty expr
            let finish = System.DateTime.Now
            let duration = finish - start
            $"[{duration.TotalMilliseconds}ms]>> {expr}"
        with
        | EvalException err -> $"!! {err}"

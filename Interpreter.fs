module Fang.Interpreter

open Fang.Lang

type Env = Env of Map<VarName, Closure>
and Closure = { env: Env; expr: Expr }

and Value =
    | Atomic of Expr
    | Composite of Closure

let emptyEnv = Env Map.empty

let isEmptyEnv (Env e) = Map.isEmpty e

let lookupVar (var: VarName) (Env e) = Map.tryFind var e

let addToEnv var closure (Env env) = Env(Map.add var closure env)

let combineEnvs (Env high) (Env low) =
    if Map.isEmpty high then
        (Env low)
    else if Map.isEmpty low then
        (Env high)
    else
        let mutable newMap = low

        for entry in high do
            newMap <- Map.add entry.Key entry.Value newMap

        Env newMap

type EvalError =
    | UnboundName of VarName
    | WrongType of Expr * expectedType: string
    | WrongValue of Value * expectedKind: string
    | Todo of Expr

exception EvalException of EvalError

let evalArithmetic fn a b =
    match fn with
    | Add -> a + b
    | Sub -> a - b
    | Mul -> a * b
    | Div -> a / b

let evalComparison fn l r =
    match fn with
    | Less -> if l < r then 1 else 0
    | Greater -> if l > r then 1 else 0
    | Equal -> if l = r then 1 else 0

let exprAsIntExn expr =
    match expr with
    | Lit (BType.Int intVal) -> intVal
    | _ -> raise (EvalException(WrongType(expr, "int")))

let exprAsLambdaExn expr =
    match expr with
    | Lam (var, body) -> (var, body)
    | expr -> raise (EvalException(WrongType(expr, "lambda")))


let valueAsAtomicExn =
    function
    | Atomic expr -> expr
    | Composite _ as value -> raise (EvalException(WrongValue(value, "atomic")))

let rec evalExn (env: Env) (expr: Expr) : Value =
    match expr with
    | Lit _ -> Atomic expr
    | Var var ->
        lookupVar var env
        |> Option.map (fun x -> evalExn x.env x.expr)
        |> Option.defaultWith (fun () -> raise (EvalException(UnboundName var)))
    | Lam _ ->
        if isEmptyEnv env then
            Atomic expr
        else
            Composite { env = env; expr = expr }
    | Builtin (Arithmetic (fn, a, b)) ->
        let a = evalExn env a |> valueAsAtomicExn |> exprAsIntExn
        let b = evalExn env b |> valueAsAtomicExn |> exprAsIntExn

        evalArithmetic fn a b
        |> BType.Int
        |> Lit
        |> Atomic
    | Builtin (Comparison (fn, l, r)) ->
        let a = evalExn env l |> valueAsAtomicExn |> exprAsIntExn
        let b = evalExn env r |> valueAsAtomicExn |> exprAsIntExn

        evalComparison fn a b
        |> BType.Int
        |> Lit
        |> Atomic
    | Bind (recursive, var, body, expr) ->
        let bodyEnv =
            if recursive then
                addToEnv var { env = env; expr = body } env
            else
                env

        let evalBody = evalExn bodyEnv body

        let exprEnv =
            match evalBody with
            | Composite closure -> addToEnv var closure env
            | Atomic expr -> addToEnv var { env = emptyEnv; expr = expr } env

        evalExn exprEnv expr
    | App (expr, arg) ->
        let arg =
            match evalExn env arg with
            | Atomic atomicExpr -> { env = emptyEnv; expr = atomicExpr }
            | Composite closure -> closure

        let closedEnv, closedExpr =
            match evalExn env expr with
            | Atomic atomicExpr -> emptyEnv, atomicExpr
            | Composite { env = env; expr = expr } -> env, expr

        let var, body = closedExpr |> exprAsLambdaExn

        let newEnv =
            addToEnv var arg (combineEnvs closedEnv env)

        evalExn newEnv body
    | Cond (pred, t, f) ->
        let pred =
            evalExn env pred |> valueAsAtomicExn |> exprAsIntExn

        if pred <> 0 then
            evalExn env t
        else
            evalExn env f

module Ex =
    let id =
        let x = VarName "x"
        Lam(x, Var x)

    let fixpoint =
        let f = VarName "f"
        let x = VarName "x"
        let v = VarName "v"

        let innerAbs =
            Lam(x, App(expr = Var f, arg = Lam(v, body = App(expr = App(expr = Var x, arg = Var x), arg = Var v))))

        Lam(f, body = App(expr = innerAbs, arg = innerAbs))

    let factorialStep =
        let f = VarName "f"
        let n = VarName "n"
        let isZero expr t f = Cond(expr, f, t)
        let one = Lit(BType.Int 1)
        let pred n = Builtin(Arithmetic(Sub, n, one))
        let mul a b = Builtin(Arithmetic(Mul, a, b))

        Lam(f, Lam(n, isZero (Var n) one (mul (Var n) (App(Var f, pred (Var n))))))

    let factorial = App(fixpoint, factorialStep)

    let fibStep =
        let f = VarName "f"
        let n = VarName "n"
        let one = Lit(BType.Int 1)
        let two = Lit(BType.Int 2)
        let leqOne = Builtin(Comparison(Less, Var n, two))

        let fn1 =
            App(Var f, Builtin(Arithmetic(Sub, Var n, one)))

        let fn2 =
            App(Var f, Builtin(Arithmetic(Sub, Var n, two)))

        Lam(f, Lam(n, Cond(leqOne, one, Builtin(Arithmetic(Add, fn1, fn2)))))

    let fib = App(fixpoint, fibStep)

    let withFibDirect cont =
        let fib = VarName "fib"
        let n = VarName "n"
        let one = Lit(BType.Int 1)
        let two = Lit(BType.Int 2)
        let leqOne = Builtin(Comparison(Less, Var n, two))

        let fn1 =
            App(Var fib, Builtin(Arithmetic(Sub, Var n, one)))

        let fn2 =
            App(Var fib, Builtin(Arithmetic(Sub, Var n, two)))

        Bind(
            recursive = true,
            var = fib,
            body = Lam(n, Cond(leqOne, one, Builtin(Arithmetic(Add, fn1, fn2)))),
            expr = cont
        )

    let ex1 n = App(id, Lit(BType.Int n))

    let ex2 n =
        let x = VarName "x"
        App(App(id, Lam(x, Builtin(Arithmetic(Mul, Var x, Lit(BType.Int 2))))), Lit(BType.Int n))

    let ex3 n = App(factorial, Lit(BType.Int n))

    let ex4 n = App(fib, Lit(BType.Int n))

    let ex5 n = withFibDirect (App(expr=Var (VarName "fib"), arg=Lit(BType.Int n)))

    let evalPrint expr =
        let start = System.DateTime.Now

        try
            let expr = evalExn emptyEnv expr
            let finish = System.DateTime.Now
            let duration = finish - start
            printfn $">> {expr} [{duration.TotalMilliseconds}ms]"
        with
        | EvalException err -> printfn $"!! {err}"

    let runExamples () =
        printfn "Warmup examples"
        evalPrint (ex1 42)
        evalPrint (ex2 15)
        evalPrint (ex3 10)
        printfn "Fib via Y-combinator"
        evalPrint (ex4 10)
        evalPrint (ex4 20)
        evalPrint (ex4 26)
        printfn "Fib via direct encoding"
        evalPrint (ex5 10)
        evalPrint (ex5 20)
        evalPrint (ex5 26)
        evalPrint (ex5 30)

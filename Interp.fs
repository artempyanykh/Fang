module Fang.Interp

open Fang.Lang

type Env = Env of List<VarName * Closure>
and Closure = { env: Env; expr: Expr }

let emptyEnv = Env List.empty

let lookupVar var (Env l) =
    List.tryFind (fun (n, _) -> n = var) l
    |> Option.map snd

let addToEnv var closure (Env env) = Env((var, closure) :: env)

let stackEnv (Env high) (Env low) = Env(List.append high low)

let dropFromEnv var (Env l) =
    Env(List.filter (fun (v, c) -> v <> var) l)

type EvalError =
    | UnboundName of VarName
    | WrongType of Expr * expectedType: string
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

let exprAsInt expr =
    match expr with
    | Literal (BType.Int intVal) -> intVal
    | _ -> raise (EvalException(WrongType(expr, "int")))

let exprAsLambda expr =
    match expr with
    | Lam (var, body) -> (var, body)
    | expr -> raise (EvalException(WrongType(expr, "lambda")))

let rec eval (env: Env) (expr: Expr) : Expr =
    match expr with
    | Literal _ -> expr
    | Var var ->
        lookupVar var env
        |> Option.map (fun x -> eval x.env x.expr)
        |> Option.defaultWith (fun () -> raise (EvalException(UnboundName var)))
    | Lam _ -> expr
    | Builtin (Arithmetic (fn, a, b)) ->
        let a = eval env a |> exprAsInt
        let b = eval env b |> exprAsInt
        evalArithmetic fn a b |> BType.Int |> Literal
    | Builtin (Comparison (fn, l, r)) ->
        let a = eval env l |> exprAsInt
        let b = eval env r |> exprAsInt
        evalComparison fn a b |> BType.Int |> Literal
    | Bind (recursive, var, body, expr) ->
        let bodyEnv =
            if recursive then
                addToEnv var { env = env; expr = body } env
            else
                env

        let body = eval bodyEnv body

        let exprEnv =
            addToEnv var { env = env; expr = body } env

        eval exprEnv expr
    | App (expr, arg) ->
        let arg = eval env arg
        let var, body = eval env expr |> exprAsLambda

        let newEnv =
            addToEnv var { env = env; expr = arg } env

        eval newEnv body
    | Cond (pred, t, f) ->
        let pred = eval env pred |> exprAsInt

        if pred <> 0 then
            eval env t
        else
            eval env f

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
        let one = Literal(BType.Int 1)
        let pred n = Builtin(Arithmetic(Sub, n, one))
        let mul a b = Builtin(Arithmetic(Mul, a, b))

        Lam(f, Lam(n, isZero (Var n) one (mul (Var n) (App(Var f, pred (Var n))))))

    let factorial = App(fixpoint, factorialStep)

    let fibStep =
        let f = VarName "f"
        let n = VarName "n"
        let one = Literal(BType.Int 1)
        let two = Literal(BType.Int 2)
        let leqOne = Builtin(Comparison(Less, Var n, two))

        let fn1 =
            App(Var f, Builtin(Arithmetic(Sub, Var n, one)))

        let fn2 =
            App(Var f, Builtin(Arithmetic(Sub, Var n, two)))

        Lam(f, Lam(n, Cond(leqOne, one, Builtin(Arithmetic(Add, fn1, fn2)))))

    let fib = App(fixpoint, fibStep)

    let ex1 n = App(id, Literal(BType.Int n))

    let ex2 n =
        let x = VarName "x"
        App(App(id, Lam(x, Builtin(Arithmetic(Mul, Var x, Literal(BType.Int 2))))), Literal(BType.Int n))

    let ex3 n = App(factorial, Literal(BType.Int n))

    let ex4 n = App(fib, Literal(BType.Int n))

    let evalPrint expr =
        let start = System.DateTime.Now

        try
            let expr = eval emptyEnv expr
            let finish = System.DateTime.Now
            let duration = finish - start
            printfn $">> {expr} [{duration.TotalMilliseconds}ms]"
        with
        | EvalException err -> printfn $"!! {err}"

    let runExamples () =
//        evalPrint (ex1 42)
//        evalPrint (ex2 15)
        evalPrint (ex3 10)
//        evalPrint (ex4 26)

module Fang.Minimal

module Lang =
    type VarName = VarName of string

    type DType =
        | TInt of int
        | TUnit of unit

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

    and Lambda = { var: VarName; body: Expr }

    and Expr =
        | Literal of DType
        | Var of VarName
        | Abs of Lambda
        | App of expr: Expr * arg: Expr
        | Cond of pred: Expr * trueBranch: Expr * falseBranch: Expr
        | Builtin of BuiltinFn


    let tryExprAsInt: Expr -> Option<int> =
        function
        | Literal (TInt value) -> Some value
        | _ -> None

    let tryExtractLambdaFromExpr: Expr -> Option<Lambda> =
        function
        | Abs lambda -> Some lambda
        | _ -> None

    let rec subst (var: VarName) (forExpr: Expr) (inExpr: Expr) : Expr =
        let substFn = subst var forExpr

        match inExpr with
        | Var name -> if name = var then forExpr else Var name
        | Abs lambda ->
            if lambda.var = var then
                inExpr
            else
                Abs
                    { lambda with
                          body = (substFn lambda.body) }
        | Literal _ -> inExpr
        | App (expr, arg) -> App(substFn expr, substFn arg)
        | Cond (pred, t, f) -> Cond(substFn pred, substFn t, substFn f)
        | Builtin (Arithmetic (fn, opA, opB)) -> Builtin(Arithmetic(fn, substFn opA, substFn opB))
        | Builtin (Comparison (fn, lhs, rhs)) -> Builtin(Comparison(fn, substFn lhs, substFn rhs))

module EvalLazy =
    open Lang
    type EvalError = string

    // "By name" lazy evaluation
    // Application is done via dumb substitution
    let rec eval (expr: Expr) : Result<Expr, EvalError> =
        match expr with
        | Literal _ -> Ok expr
        | Abs _ -> Ok expr
        | Var name -> Error $"Use of unbound variable: {name}"
        | Cond (pred, t, f) ->
            let evaluatedPred = eval pred

            let evalCond cond =
                match tryExprAsInt cond with
                | Some 0 -> eval f
                | Some _ -> eval t
                | None -> Error $"Expected an integer valued predicate: {cond}"

            evaluatedPred |> Result.bind evalCond
        | App (expr, arg) ->
            let evalExpr = eval expr

            let evalLambda expr =
                match tryExtractLambdaFromExpr expr with
                | None -> Error $"Expected a lambda in application: {expr}"
                | Some lambda -> subst lambda.var arg lambda.body |> eval

            evalExpr |> Result.bind evalLambda
        | Builtin (Arithmetic (fn, opA, opB)) ->
            let evaluatedOpA = eval opA
            let evaluatedOpB = eval opB

            let evalFn exprA exprB =
                let valA = tryExprAsInt exprA
                let valB = tryExprAsInt exprB

                match valA, valB with
                | None, _ -> Error $"Expected an integer operand to {fn}: {exprA}"
                | _, None -> Error $"Expected an integer operand to {fn}: {exprB}"
                | Some valA, Some valB ->
                    let value =
                        match fn with
                        | Add -> valA + valB
                        | Sub -> valA - valB
                        | Mul -> valA * valB
                        | Div -> valA / valB

                    Ok(Literal(TInt value))

            match evaluatedOpA, evaluatedOpB with
            | Ok opA, Ok opB -> evalFn opA opB
            | Error errA, _ -> Error errA
            | _, Error errB -> Error errB
        | Builtin (Comparison (fn, lhs, rhs)) ->
            let evaluatedLhs = eval lhs
            let evaluatedRhs = eval rhs

            let evalFn lhsExpr rhsExpr =
                let valLhs = tryExprAsInt lhsExpr
                let valRhs = tryExprAsInt rhsExpr

                match valLhs, valRhs with
                | None, _ -> Error $"Expected an integer operand to {fn}: {lhsExpr}"
                | _, None -> Error $"Expected an integer operand to {fn}: {rhsExpr}"
                | Some valA, Some valB ->
                    let value =
                        match fn with
                        | Less -> if valA < valB then 1 else 0
                        | Equal -> if valA = valB then 1 else 0
                        | Greater -> if valA > valB then 1 else 0

                    Ok(Literal(TInt value))

            match evaluatedLhs, evaluatedRhs with
            | Ok lhs, Ok rhs -> evalFn lhs rhs
            | Error errA, _ -> Error errA
            | _, Error errB -> Error errB


    let idExpr =
        let x = VarName "x"

        Abs { var = x; body = Var x }

    let incrementExpr =
        let x = VarName "x"

        Abs
            { var = x
              body = Builtin(Arithmetic(fn = Add, opA = (Var x), opB = (Literal(TInt 1)))) }

    let fixpoint =
        let f = VarName "f"
        let x = VarName "x"

        let innerAbs =
            Abs
                { var = x
                  body = App(expr = Var f, arg = App(expr = Var x, arg = Var x)) }

        Abs
            { var = f
              body = App(expr = innerAbs, arg = innerAbs) }

    let factorialStep =
        let f = VarName "f"
        let n = VarName "n"
        let isZero expr t f = Cond(expr, f, t)
        let one = Literal(TInt 1)
        let pred n = Builtin(Arithmetic(Sub, n, one))
        let mul a b = Builtin(Arithmetic(Mul, a, b))

        Abs
            { var = f
              body =
                  Abs
                      { var = n
                        body = isZero (Var n) one (mul (Var n) (App(Var f, pred (Var n)))) } }

    let factorial = App(fixpoint, factorialStep)

    let fibStep =
        let f = VarName "f"
        let n = VarName "n"
        let one = Literal(TInt 1)
        let two = Literal(TInt 2)
        let leqOne = Builtin(Comparison(Less, Var n, two))

        let fn1 =
            App(Var f, Builtin(Arithmetic(Sub, Var n, one)))

        let fn2 =
            App(Var f, Builtin(Arithmetic(Sub, Var n, two)))

        Abs
            { var = f
              body =
                  Abs
                      { var = n
                        body = Cond(leqOne, one, Builtin(Arithmetic(Add, fn1, fn2))) } }

    let fib = App(fixpoint, fibStep)

    let ex1 = App(incrementExpr, Literal(TInt 42))
    let ex2 = App(idExpr, Literal(TInt 23))
    let ex3 = App(factorial, Literal(TInt 10))

    let ex4 n = App(fib, Literal(TInt n))

    let evalPrint expr =
        let start = System.DateTime.Now

        match eval expr with
        | Ok expr ->
            let finish = System.DateTime.Now
            let duration = finish - start
            printfn $">> {expr} [{duration.Milliseconds}ms]"
        | Error msg -> printfn $"!! {msg}"

    let runExamples () =
        evalPrint ex1
        evalPrint ex2
        evalPrint ex3
        evalPrint (ex4 26)

module EvalEager =
    open Lang
    type EvalError = string

    // "By value" eager evaluation
    // Application is done via dumb substitution
    let rec eval (expr: Expr) : Result<Expr, EvalError> =
        match expr with
        | Literal _ -> Ok expr
        | Abs _ -> Ok expr
        | Var name -> Error $"Use of unbound variable: {name}"
        | Cond (pred, t, f) ->
            let evaluatedPred = eval pred

            let evalCond cond =
                match tryExprAsInt cond with
                | Some 0 -> eval f
                | Some _ -> eval t
                | None -> Error $"Expected an integer valued predicate: {cond}"

            evaluatedPred |> Result.bind evalCond
        | App (expr, arg) ->
            let evalExpr = eval expr
            let evalArg = eval arg

            let evalLambda expr arg =
                match tryExtractLambdaFromExpr expr with
                | None -> Error $"Expected a lambda in application: {expr}"
                | Some lambda -> subst lambda.var arg lambda.body |> eval

            match evalExpr, evalArg with
            | Ok expr, Ok arg -> evalLambda expr arg
            | Error e, _ -> Error e
            | _, Error e -> Error e
        | Builtin (Arithmetic (fn, opA, opB)) ->
            let evaluatedOpA = eval opA
            let evaluatedOpB = eval opB

            let evalFn exprA exprB =
                let valA = tryExprAsInt exprA
                let valB = tryExprAsInt exprB

                match valA, valB with
                | None, _ -> Error $"Expected an integer operand to {fn}: {exprA}"
                | _, None -> Error $"Expected an integer operand to {fn}: {exprB}"
                | Some valA, Some valB ->
                    let value =
                        match fn with
                        | Add -> valA + valB
                        | Sub -> valA - valB
                        | Mul -> valA * valB
                        | Div -> valA / valB

                    Ok(Literal(TInt value))

            match evaluatedOpA, evaluatedOpB with
            | Ok opA, Ok opB -> evalFn opA opB
            | Error errA, _ -> Error errA
            | _, Error errB -> Error errB
        | Builtin (Comparison (fn, lhs, rhs)) ->
            let evaluatedLhs = eval lhs
            let evaluatedRhs = eval rhs

            let evalFn lhsExpr rhsExpr =
                let valLhs = tryExprAsInt lhsExpr
                let valRhs = tryExprAsInt rhsExpr

                match valLhs, valRhs with
                | None, _ -> Error $"Expected an integer operand to {fn}: {lhsExpr}"
                | _, None -> Error $"Expected an integer operand to {fn}: {rhsExpr}"
                | Some valA, Some valB ->
                    let value =
                        match fn with
                        | Less -> if valA < valB then 1 else 0
                        | Equal -> if valA = valB then 1 else 0
                        | Greater -> if valA > valB then 1 else 0

                    Ok(Literal(TInt value))

            match evaluatedLhs, evaluatedRhs with
            | Ok lhs, Ok rhs -> evalFn lhs rhs
            | Error errA, _ -> Error errA
            | _, Error errB -> Error errB


    let idExpr =
        let x = VarName "x"

        Abs { var = x; body = Var x }

    let incrementExpr =
        let x = VarName "x"

        Abs
            { var = x
              body = Builtin(Arithmetic(fn = Add, opA = (Var x), opB = (Literal(TInt 1)))) }

    let fixpoint =
        let f = VarName "f"
        let x = VarName "x"
        let v = VarName "v"

        let innerAbs =
            Abs
                { var = x
                  body =
                      App(
                          expr = Var f,
                          arg =
                              Abs
                                  { var = v
                                    body = App(expr = App(expr = Var x, arg = Var x), arg = Var v) }
                      ) }

        Abs
            { var = f
              body = App(expr = innerAbs, arg = innerAbs) }

    let factorialStep =
        let f = VarName "f"
        let n = VarName "n"
        let isZero expr t f = Cond(expr, f, t)
        let one = Literal(TInt 1)
        let pred n = Builtin(Arithmetic(Sub, n, one))
        let mul a b = Builtin(Arithmetic(Mul, a, b))

        Abs
            { var = f
              body =
                  Abs
                      { var = n
                        body = isZero (Var n) one (mul (Var n) (App(Var f, pred (Var n)))) } }

    let factorial = App(fixpoint, factorialStep)

    let fibStep =
        let f = VarName "f"
        let n = VarName "n"
        let one = Literal(TInt 1)
        let two = Literal(TInt 2)
        let leqOne = Builtin(Comparison(Less, Var n, two))

        let fn1 =
            App(Var f, Builtin(Arithmetic(Sub, Var n, one)))

        let fn2 =
            App(Var f, Builtin(Arithmetic(Sub, Var n, two)))

        Abs
            { var = f
              body =
                  Abs
                      { var = n
                        body = Cond(leqOne, one, Builtin(Arithmetic(Add, fn1, fn2))) } }

    let fib = App(fixpoint, fibStep)

    let ex1 = App(incrementExpr, Literal(TInt 42))
    let ex2 = App(idExpr, Literal(TInt 23))
    let ex3 = App(factorial, Literal(TInt 10))

    let ex4 n = App(fib, Literal(TInt n))

    let evalPrint expr =
        let start = System.DateTime.Now

        match eval expr with
        | Ok expr ->
            let finish = System.DateTime.Now
            let duration = finish - start
            printfn $">> {expr} [{duration.Milliseconds}ms]"
        | Error msg -> printfn $"!! {msg}"

    let runExamples () =
        evalPrint ex1
        evalPrint ex2
        evalPrint ex3
        evalPrint (ex4 26)

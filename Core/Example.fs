module Fang.Example

open Fang.Lang

module AST =
    let arithmetic =
        // 2 + 3 * 4
        Builtin(
            Arithmetic( //
                Add,
                Lit(BType.Int 2),
                Builtin(
                    Arithmetic( //
                        Mul,
                        Lit(BType.Int 3),
                        Lit(BType.Int 4)
                    )
                )
            )
        )

    let arithmeticBinds =
        // let x = 2 + 3 * 4 in let y = 42 in x - y
        Bind(
            recursive = false,
            var = VarName.make "x",
            body = arithmetic,
            expr =
                Bind(
                    recursive = false,
                    var = VarName.make "y",
                    body = Lit(BType.Int 42),
                    expr = Builtin(Arithmetic(Sub, Var(VarName.make "x"), Var(VarName.make "y")))
                )
        )

    let plus42 =
        Bind(
            recursive = false,
            var = VarName.make "plus42",
            body = Abs(VarName.make "x", Builtin(Arithmetic(Add, Var(VarName.make "x"), Lit(BType.Int 42)))),
            expr = App(Var(VarName.make "plus42"), Lit(BType.Int 3))
        )

    let plusN =
        Bind(
            recursive = false,
            var = VarName.make "plusN",
            body =
                Abs(
                    VarName.make "N",
                    Abs(VarName.make "x", Builtin(Arithmetic(Add, Var(VarName.make "x"), Var(VarName.make "N"))))
                ),
            expr = App(App(Var(VarName.make "plusN"), Lit(BType.Int 3)), Lit(BType.Int 5))
        )


    let fibDirect (num: int) =
        let fib = VarName.make "fib"
        let n = VarName.make "n"
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
            body = Abs(n, Cond(leqOne, one, Builtin(Arithmetic(Add, fn1, fn2)))),
            expr = App(Var fib, Lit(BType.Int num))
        )

    let fixpointOperator =
        let f = VarName.make "f"
        let x = VarName.make "x"
        let v = VarName.make "v"

        let innerAbs =
            Abs(x, App(expr = Var f, arg = Abs(v, body = App(expr = App(expr = Var x, arg = Var x), arg = Var v))))

        Abs(f, body = App(expr = innerAbs, arg = innerAbs))

    let fibStep =
        let f = VarName.make "f"
        let n = VarName.make "n"
        let one = Lit(BType.Int 1)
        let two = Lit(BType.Int 2)
        let leqOne = Builtin(Comparison(Less, Var n, two))

        let fn1 =
            App(Var f, Builtin(Arithmetic(Sub, Var n, one)))

        let fn2 =
            App(Var f, Builtin(Arithmetic(Sub, Var n, two)))

        Abs(f, Abs(n, Cond(leqOne, one, Builtin(Arithmetic(Add, fn1, fn2)))))

    let fibFixpoint (n: int) =
        App(App(fixpointOperator, fibStep), (Lit(BType.Int n)))

    let badRecursiveBind =
        Bind(recursive = true, var = "x", body = Builtin(Arithmetic(Add, Lit(BType.Int 1), Var "x")), expr = Var "x")

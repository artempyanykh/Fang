module Fang.Example

module AST =
    open Lang
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
            var = VarName "x",
            body = arithmetic,
            expr =
                Bind(
                    recursive = false,
                    var = VarName "y",
                    body = Lit(BType.Int 42),
                    expr = Builtin(Arithmetic(Sub, Var(VarName "x"), Var(VarName "y")))
                )
        )

    let plus42 =
        Bind(
            recursive = false,
            var = VarName "plus42",
            body = Lam(VarName "x", Builtin(Arithmetic(Add, Var(VarName "x"), Lit(BType.Int 42)))),
            expr = App(Var(VarName "plus42"), Lit(BType.Int 3))
        )

    let plusN =
        Bind(
            recursive = false,
            var = VarName "plusN",
            body = Lam(VarName "N", Lam(VarName "x", Builtin(Arithmetic(Add, Var(VarName "x"), Var(VarName "N"))))),
            expr = App(App(Var(VarName "plusN"), Lit(BType.Int 3)), Lit(BType.Int 5))
        )


    let fib (num: int) =
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
            expr = App(Var fib, Lit(BType.Int num))
        )

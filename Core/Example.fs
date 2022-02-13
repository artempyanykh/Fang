module Fang.Example

open Fang.Lang

module AST =
    let arithmetic = Parser.parse "2 + 3 * 4"

    let arithmeticBinds =
        Parser.parse "let x = 2 + 3 * 4 in let y = 42 in x - y"

    let plus42 =
        Parser.parse "let plus42 x = x + 42 in plus42 3"

    let plusN =
        Parser.parse "let plusN N x = x + N in plusN 3 5"


    let fibDirect (num: int) =
        Parser.parse $"let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2) in fib {num}"

    let fixpointOperator =
        let source =
            @"\f. (\x. f (\v. x x v)) (\x. f (\v. x x v))"

        Parser.parse source


    let fibStep =
        let source =
            @"\f. \n. if n < 2 then 1 else f (n - 1) + f (n - 2)"

        Parser.parse source

    let fibFixpoint (n: int) =
        App(App(fixpointOperator, fibStep), (Lit(BType.Int n)))

    let badRecursiveBind = Parser.parse "let rec x = 1 + x in x"

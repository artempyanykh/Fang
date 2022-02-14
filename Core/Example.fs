module Fang.Example

open Fang.Lang
open Fang.Parser

module AST =
    let arithmetic = parse "2 + 3 * 4"

    let arithmeticBinds =
        parse "let x = 2 + 3 * 4 in let y = 42 in x - y"

    let plus42 =
        parse "let plus42 x = x + 42 in plus42 3"

    let plusN =
        parse "let plusN N x = x + N in plusN 3 5"


    let fibDirect (num: int) =
        parse $"let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2) in fib {num}"

    let fixpointOperator =
        let source =
            @"\f. (\x. f (\v. x x v)) (\x. f (\v. x x v))"

        parse source


    let fibStep =
        let source =
            @"\f. \n. if n < 2 then 1 else f (n - 1) + f (n - 2)"

        parse source

    let fibFixpoint (n: int) =
        App(App(fixpointOperator, fibStep), (Lit(BType.Int n)))

    let badRecursiveBind = parse "let rec x = 1 + x in x"

module Fang.Tests

open System
open Snapper
open Snapper.Attributes
open Xunit
open Parser

[<StoreSnapshotsPerClass>]
module ParserTests =

    [<Fact>]
    let ``nop`` () =
        let r = tryParse ""
        Assert.Equal(r, None)

    [<Fact>]
    let num () = (tryParse "1").ShouldMatchSnapshot()

    [<Fact>]
    let negNum () = (tryParse "-1").ShouldMatchSnapshot()

    [<Fact>]
    let opAdd () = (tryParse "1+2").ShouldMatchSnapshot()

    [<Fact>]
    let opSub () = (tryParse "1-5").ShouldMatchSnapshot()

    [<Fact>]
    let subComplex () =
        (tryParse "-(42-17)").ShouldMatchSnapshot()

    [<Fact>]
    let doubleNeg () =
        let _, errors = tryParseWithDiag "--6"
        errors.ShouldMatchSnapshot()

    [<Fact>]
    let plusMulMixed () =
        (tryParse "2 + 3 * 4").ShouldMatchSnapshot()

    [<Fact>]
    let letWithoutIn () =
        let _, errors = tryParseWithDiag "let x = 5"
        errors.ShouldMatchSnapshot()

    [<Fact>]
    let letWithoutIn2 () =
        let source =
            @"let x = 5
let y = 6"

        let _, errors = tryParseWithDiag source
        errors.ShouldMatchSnapshot()

    [<Fact>]
    let simpleAbs () =
        (tryParse @"\n. n").ShouldMatchSnapshot()

    [<Fact>]
    let multiArgAbs () =
        (tryParse @"\a b c. b*b - 4*a*c")
            .ShouldMatchSnapshot()

    [<Fact>]
    let letInArithmetic () =
        let source = @"let inc x = x + 1 in inc 3"
        (tryParse source).ShouldMatchSnapshot()

    [<Fact>]
    let digitsInIdents () =
        (tryParse "let x2 = 42 in x2")
            .ShouldMatchSnapshot()

    [<Fact>]
    let fib () =
        let ast =
            tryParse (@"let rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2) in fib 10")

        ast.ShouldMatchSnapshot()

[<StoreSnapshotsPerClass>]
module ParserV2Tests =
    open Parser.V2

    let checkParseSnapshot src =
        match tryParseWithDiag src with
        | Some expr, [] ->
            let repr = expr.ToString()

            let lines =
                repr.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)

            lines.ShouldMatchSnapshot()
        | _, errors -> errors.ShouldMatchSnapshot()

    [<Fact>]
    let ``nop`` () =
        let r = tryParse ""
        Assert.Equal(r, None)

    [<Fact>]
    let num () = checkParseSnapshot "1"

    [<Fact>]
    let negNum () = checkParseSnapshot "-1"

    [<Fact>]
    let opAdd () = checkParseSnapshot "1+2"

    [<Fact>]
    let opSub () = checkParseSnapshot "1-5"

    [<Fact>]
    let subComplex () = checkParseSnapshot "-(42-17)"

    [<Fact>]
    let doubleNeg () = checkParseSnapshot "--6"

    [<Fact>]
    let plusMulMixed () = checkParseSnapshot "2 + 3 * 4"

    [<Fact>]
    let letWithoutIn () = checkParseSnapshot "let x = 5"

    [<Fact>]
    let letWithoutIn2 () =
        let source =
            @"let x = 5
let y = 6"

        checkParseSnapshot source

    [<Fact>]
    let simpleAbs () = checkParseSnapshot @"\n. n"

    [<Fact>]
    let multiArgAbs () =
        checkParseSnapshot @"\a b c. b*b - 4*a*c"
        
    [<Fact>]
    let absAndLet () =
        checkParseSnapshot @"let f = \x. x + 2 in f 2"

    [<Fact>]
    let letInArithmetic () =
        let source = @"let inc x = x + 1 in inc 3"
        checkParseSnapshot source

    [<Fact>]
    let digitsInIdents () = checkParseSnapshot "let x2 = 42 in x2"


    [<Fact>]
    let fib () =
        checkParseSnapshot @"let rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2) in fib 10"

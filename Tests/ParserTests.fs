module Fang.Tests

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
    let fib () =
        let ast =
            tryParse (@"let rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2) in fib 10")

        ast.ShouldMatchSnapshot()

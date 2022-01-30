module Fang.Tests

open Snapshooter.Xunit
open Xunit

module ParserTests =
    open Lang
    open Parser

    [<Fact>]
    let ``nop`` () =
        let r = tryParse ""
        Assert.Equal(r, None)

    [<Fact>]
    let num () = Assert.Equal(None, tryParse "1")

    [<Fact>]
    let negNum () = Assert.Equal(None, tryParse "-1")

    [<Fact>]
    let opAdd () = Assert.Equal(None, tryParse "1+2")

    [<Fact>]
    let opSub () = Assert.Equal(None, tryParse "1-5")

    let subComplex () = Assert.Equal(None, tryParse "-(42-17)")
    
//    [<Fact>]
//    let doubleNeg () = Assert.Rai(None, tryParse("--6"))
    
    [<Fact>]
    let simpleLet () =
        Assert.Equal(None, tryParse "let x = 5")
        List.fold
    
    [<Fact>]
    let simpleAbs () =
        Assert.Equal(None, tryParse @"\n. n")
        
    [<Fact>]
    let multiArgAbs () =
        Assert.Equal(None, tryParse @"\a b c. b*b - 4*a*c")
    
    [<Fact>]
    let fib () =
        let ast = tryParse(@"let rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2) in fib 10")
        Assert.Equal(None, ast)
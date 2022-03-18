namespace Fang

open System
open Fang.Ir
open Snapper
open Snapper.Attributes
open Xunit

open Parser

[<StoreSnapshotsPerClass>]
module IrTests =
    let toLines (ir: Hir.Module) =
        let lines =
            seq {
                for phrase in ir do
                    let repr = phrase.ToString()
                    yield! repr.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)
            }

        lines

    [<Fact>]
    let ``value binding 1`` () =
        let expr = parse "let x = 42 in x"
        let ir = Hir.translateExpr expr
        (toLines ir).ShouldMatchSnapshot()

    [<Fact>]
    let ``shadowing`` () =
        let expr = parse "let x = 42 in let x = x + 1 in x"
        let ir = Hir.translateExpr expr
        (toLines ir).ShouldMatchSnapshot()

    [<Fact>]
    let ``let function 1`` () =
        let expr = parse "let inc x = x + 1 in inc 9"
        let ir = Hir.translateExpr expr
        (toLines ir).ShouldMatchSnapshot()

    [<Fact>]
    let ``let function 2`` () =
        let expr = parse "let add x y = x + y in add 4 16"
        let ir = Hir.translateExpr expr
        (toLines ir).ShouldMatchSnapshot()

    [<Fact>]
    let ``fun state no capture`` () =
        let expr =
            parse "let addSurprise x = let surprise = 42 in  x + surprise in addSurprise 0"

        let ir = Hir.translateExpr expr
        (toLines ir).ShouldMatchSnapshot()

    [<Fact>]
    let ``fun capture`` () =
        let expr =
            parse "let surprise = 42 in let addSurprise x = x + surprise in addSurprise 0"

        let ir = Hir.translateExpr expr
        (toLines ir).ShouldMatchSnapshot()

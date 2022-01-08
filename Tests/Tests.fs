module Fang.Tests

open Xunit

module TokenizerTests =
    module Parser = ParserFacade

    [<Fact>]
    let ``nop``() =
        let r = Parser.tryParse ""
        Assert.Equal(r, None)

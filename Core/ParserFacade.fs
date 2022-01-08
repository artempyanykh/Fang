module Fang.ParserFacade


open FSharp.Text.Lexing
open Fang.Lang

let tryParse (s: string) : Option<Expr> =
    let buf = LexBuffer<char>.FromString s
    let res = Parser.start Lexer.token buf
    res


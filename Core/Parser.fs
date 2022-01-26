module Fang.Parser


open FSharp.Text.Lexing
open Fang.Lang

let tryParse (s: string) : Option<Expr> =
    let buf = LexBuffer<char>.FromString s
    let res = GeneratedParser.start GeneratedLexer.token buf
    res


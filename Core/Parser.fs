module Fang.Parser


open FSharp.Text.Lexing
open FSharp.Text.Parsing
open Fang.Lang

type token = GeneratedParser.token

let tryLex (s: string) : List<GeneratedParser.token> =
    let buf = LexBuffer<char>.FromString s
    let tokens = ResizeArray()
    let mutable isDone = false
    while not isDone do
        let token = GeneratedLexer.token buf
        if token = GeneratedParser.EOF then
            isDone <- true
        else
            tokens.Add(token)
            
    List.ofSeq tokens
let tryParse (s: string) : Option<Expr> =
    let buf = LexBuffer<char>.FromString s
    try
        let res = GeneratedParser.start GeneratedLexer.token buf
        res
    with :? RecoverableParseError as e ->
        None
    
let parse (s: string) = Option.get (tryParse s)


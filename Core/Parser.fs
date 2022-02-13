module Fang.Parser


open System.IO
open FSharp.Text.Lexing
open FSharp.Text.Parsing
open Fang.Lang

type token = GeneratedParser.token

type ParseError =
    | ParseError of Option<token>
    member this.Token =
        match this with
        | ParseError token -> token

    override this.ToString() =
        match this.Token with
        | Some tok ->
            let tokName = GeneratedParser.token_to_string tok
            $"Unexpected token: {tokName}"
        | None -> "Unexpected parsing error happened. All hope is lost!"

exception ParseException of errors: List<ParseError> with
    override this.Message =
        use writer = new StringWriter()

        for e in this.errors do
            writer.WriteLine($"{e.ToString()}")

        writer.ToString()


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

let tryParseWithDiag (s: string) : Option<Expr> * List<ParseError> =
    let buf = LexBuffer<char>.FromString s
    Diagnostics.reset_parse_errors ()

    let parsed =
        try
            GeneratedParser.start GeneratedLexer.token buf
        with
        | Failure (_) -> None

    let errors =
        List.ofSeq Diagnostics.parse_errors
        |> List.map
            (function
            | Diagnostics.UntypedParseError x -> x :?> ParseErrorContext<GeneratedParser.token>)
        |> List.map (fun x -> ParseError x.CurrentToken)

    parsed, errors

let tryParse s : Option<Expr> =
    match tryParseWithDiag s with
    | Some tree, [] -> Some tree
    | None, [] -> None
    | _, errors -> raise (ParseException errors)

let parse (s: string) : Expr = Option.get (tryParse s)

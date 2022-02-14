module Fang.Parser


open System.IO
open FSharp.Text.Lexing
open FSharp.Text.Parsing
open Fang.Lang
open Farkle.Builder.OperatorPrecedence

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

module V2 =
    open Farkle
    open Farkle.Builder

    let handleNeg expr =
        match expr with
        | Lit (BType.Int i) -> Lit(BType.Int(-i))
        | _ -> Builtin(UnaryArithmetic(Neg, expr))

    let rec mkApp expr args =
        match args with
        | [ only ] -> App(expr, only)
        | h :: t -> mkApp (App(expr, h)) t
        | [] -> failwith "empty arg list"

    let rec mkBind isRec name args body expr =
        let core = body

        let unsugaredBody =
            List.foldBack (fun n acc -> Abs(n, acc)) args core

        Bind(isRec, name, unsugaredBody, expr)

    let rec mkAbs names body =
        List.foldBack (fun n acc -> Abs(n, acc)) names body

    let intNum =
        Terminals.unsignedIntRegex
        |> terminal "int" (T(fun _ s -> int (s.ToString())))

    let ident =
        Regex.regexString @"[_a-zA-Z][_a-zA-Z0-9']*"
        |> terminal "ident" (T(fun _ s -> VarName.make (s.ToString())))

    let idents = many1 ident

    let expr =
        let P_APP = obj ()
        let P_NEG = obj ()
        let P_SIMPLE = obj ()

        let simpleExpr: Nonterminal<Expr> = nonterminal "simple expr"
        let simpleExprs = many1 simpleExpr

        let expr: Nonterminal<Expr> = nonterminal "expr"

        let absExpr =
            "abstraction"
            ||= [ !& @"\" .>>. idents .>> "." .>>. expr => mkAbs ]

        let condExpr =
            "cond"
            ||= [ !& "if" .>>. expr .>> "then" .>>. expr .>> "else"
                  .>>. expr
                  => (fun pred t f -> Cond(pred, t, f)) ]

        let letExpr =
            "let"
            ||= [ !& "let" .>>. idents .>> "=" .>>. expr .>> "in"
                  .>>. expr
                  => (fun ids body -> mkBind false (List.head ids) (List.tail ids) body)
                  !& "let" .>> "rec" .>>. idents .>> "=" .>>. expr
                  .>> "in"
                  .>>. expr
                  => (fun ids body -> mkBind true (List.head ids) (List.tail ids) body) ]

        let appExpr =
            "application"
            ||= [ !@simpleExpr .>>. simpleExprs |> prec P_APP
                  => mkApp ]

        let prefixExpr =
            "prefix expr"
            ||= [ !& "-" .>>. simpleExpr |> prec P_NEG => handleNeg ]

        let infixExpr =
            "infix expr"
            ||= [ !@expr .>> "+" .>>. expr
                  => (fun l r -> Builtin(Arithmetic(ArithmeticFn.Add, l, r)))
                  !@expr .>> "-" .>>. expr
                  => (fun l r -> Builtin(Arithmetic(ArithmeticFn.Sub, l, r)))
                  !@expr .>> "*" .>>. expr
                  => (fun l r -> Builtin(Arithmetic(ArithmeticFn.Mul, l, r)))
                  !@expr .>> "/" .>>. expr
                  => (fun l r -> Builtin(Arithmetic(ArithmeticFn.Div, l, r)))

                  !@expr .>> "=" .>>. expr
                  => (fun l r -> Builtin(Comparison(ComparisonFn.Equal, l, r)))
                  !@expr .>> "<" .>>. expr
                  => (fun l r -> Builtin(Comparison(ComparisonFn.Less, l, r)))
                  !@expr .>> ">" .>>. expr
                  => (fun l r -> Builtin(Comparison(ComparisonFn.Greater, l, r))) ]

        simpleExpr.SetProductions(
            !@intNum => (fun i -> Lit(BType.Int i)), //
            !@ident => Var,
            !& "(" .>>. expr .>> ")" |> asIs
        )

        expr.SetProductions(
            !@simpleExpr |> asIs,
            !@absExpr |> asIs,
            !@condExpr |> asIs,
            !@letExpr |> asIs,
            !@appExpr |> asIs,
            !@prefixExpr |> asIs,
            !@infixExpr |> asIs
        )

        let opScope =
            OperatorScope(
                LeftAssociative("if", "then", "else", "let", "in"),
                RightAssociative(@"\", "."),
                LeftAssociative("=", "<", ">"),
                LeftAssociative("+", "-"),
                LeftAssociative("*", "/"),
                LeftAssociative(P_APP),
                PrecedenceOnly(P_NEG)
            )

        DesigntimeFarkle.withOperatorScope opScope expr

    let prog = opt expr

    exception ParseException of msg: string with
        override this.Message = this.msg

    let parser = RuntimeFarkle.build prog

    let tryParse (s: string) : Option<Expr> =
        match RuntimeFarkle.parseString parser s with
        | Ok expr -> expr
        | Error err -> raise (ParseException(err.ToString()))

    let parse (s: string) : Expr = Option.get (tryParse s)

    let tryParseWithDiag (s: string) : Option<Expr> * List<string> =
        match RuntimeFarkle.parseString parser s with
        | Ok expr -> expr, []
        | Error err -> None, [ err.ToString() ]

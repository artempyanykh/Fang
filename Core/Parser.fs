module Fang.Parser

open Fang.Lang
open Farkle.Builder.OperatorPrecedence

module internal Impl =
    open Farkle
    open Farkle.Builder

    let handleNeg expr =
        match expr with
        | Lit (BType.Int i) -> Lit(BType.Int(-i))
        | _ -> Prim(UArith(Neg, expr))

    let rec mkApp expr args =
        match args with
        | [ only ] -> App(expr, only)
        | h :: t -> mkApp (App(expr, h)) t
        | [] -> failwith "empty arg list"

    let rec mkBind isRec name args body expr =
        let core = body

        let unsugaredBody =
            List.foldBack (fun n acc -> Lam(n, acc)) args core

        Bind(isRec, name, unsugaredBody, expr)

    let rec mkLam names body =
        List.foldBack (fun n acc -> Lam(n, acc)) names body

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

        let lamExpr =
            "lambda abstraction"
            ||= [ !& @"\" .>>. idents .>> "." .>>. expr => mkLam ]

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
                  => (fun l r -> Prim(BArith(BArithFn.Add, l, r)))
                  !@expr .>> "-" .>>. expr
                  => (fun l r -> Prim(BArith(BArithFn.Sub, l, r)))
                  !@expr .>> "*" .>>. expr
                  => (fun l r -> Prim(BArith(BArithFn.Mul, l, r)))
                  !@expr .>> "/" .>>. expr
                  => (fun l r -> Prim(BArith(BArithFn.Div, l, r)))

                  !@expr .>> "=" .>>. expr
                  => (fun l r -> Prim(Cmp(CmpFn.Eq, l, r)))
                  !@expr .>> "<" .>>. expr
                  => (fun l r -> Prim(Cmp(CmpFn.Lt, l, r)))
                  !@expr .>> ">" .>>. expr
                  => (fun l r -> Prim(Cmp(CmpFn.Gt, l, r))) ]

        simpleExpr.SetProductions(
            !@intNum => (fun i -> Lit(BType.Int i)), //
            !@ident => Var,
            !& "(" .>>. expr .>> ")" |> asIs
        )

        expr.SetProductions(
            !@simpleExpr |> asIs,
            !@lamExpr |> asIs,
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


    let runtimeParser = RuntimeFarkle.build prog

    let parseWithRuntimeParser (s: string) : Result<Option<Expr>, FarkleError> =
        RuntimeFarkle.parseString runtimeParser s

exception ParseException of msg: string with
    override this.Message = this.msg

let tryParse (s: string) : Option<Expr> =
    match Impl.parseWithRuntimeParser s with
    | Ok expr -> expr
    | Error err -> raise (ParseException(err.ToString()))

let parse (s: string) : Expr = Option.get (tryParse s)

let tryParseWithDiag (s: string) : Option<Expr> * List<string> =
    match Impl.parseWithRuntimeParser s with
    | Ok expr -> expr, []
    | Error err -> None, [ err.ToString() ]

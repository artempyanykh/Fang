module Fang.Ir

module L = Lang

module Hir =
    type VarName = string

    [<RequireQualifiedAccess>]
    type Var =
        | Local of VarName
        | Global of VarName
        | Env of VarName

    type Access =
        | Public
        | Private

    type Module = array<Phrase>

    // How to support:
    // 1. Regular top-level functions
    // 2. Top-level values that may require evaluating code to be defined
    // 3. Regular expressions that need to be executed
    and Phrase =
        | Define of Function
        | Exec of Expr

    and Function =
        { isRec: bool
          name: VarName
          env: array<VarName>
          pars: array<VarName>
          body: Expr }

    and Expr =
        | Lit of L.BType
        | Var of Var
        // Captured variables can be locals only.
        // We don't need to capture globals as we can access them in another way
        | Closure of code: VarName * env: array<VarName>
        | App of fn: Expr * arg: Expr
        | Cond of p: Expr * tb: Expr * fb: Expr
        | Bind of BindLocal
        | Prim of PrimFn

    and BindLocal =
        { isRec: bool
          name: VarName
          body: Expr
          cont: Expr }

    and PrimFn =
        | BArith of fn: L.BArithFn * Expr * Expr
        | UArith of fn: L.UArithFn * Expr
        | Cmp of fn: L.CmpFn * Expr * Expr

    type Context =
        { locals: List<VarName>
          env: List<VarName>
          globals: Set<VarName> }

    module Context =
        let empty: Context =
            { locals = []
              env = []
              globals = Set.empty }

        let makeVar (ctx: Context) (name: VarName) : Var =
            match List.tryFind (fun x -> x = name) ctx.locals with
            | Some _ -> Var.Local name
            | _ ->
                match List.tryFind (fun x -> x = name) ctx.env with
                | Some _ -> Var.Env name
                | _ ->
                    if not (Set.contains name ctx.globals) then
                        failwithf $"Unknown var {name}.\n  Context: {ctx}"

                    Var.Global name

        let filterFreeVars (ctx: Context) (vars: Set<VarName>) =
            Set.difference
                vars
                (Set.unionMany [ Set.ofList ctx.locals
                                 Set.ofList ctx.env
                                 ctx.globals ])

    type Builder(expr: L.Expr) =
        let mutable nameCnt = 0
        let mutable names: Set<VarName> = Set.empty

        let phrases: ResizeArray<Phrase> = ResizeArray()

        let makeUniqueName (baseName: VarName) =
            let newName =
                if names.Contains baseName then
                    let newName = $"{baseName}_{nameCnt}"
                    nameCnt <- nameCnt + 1
                    newName
                else
                    baseName

            newName


        let rec buildFunction (ctx: Context) (name: VarName) (vars: List<VarName>) (body: L.Expr) : VarName =
            let funName = makeUniqueName name
            let bodyCtx = { ctx with locals = vars }
            let bodyExpr = translateExpr bodyCtx body
            // Right now we don't really build recursive functions
            let fn =
                { isRec = false
                  name = funName
                  env = List.toArray ctx.env
                  pars = List.toArray vars
                  body = bodyExpr }

            phrases.Add(Define fn)

            funName

        and translateLam (ctx: Context) (name: VarName) (vars: List<VarName>) (body: L.Expr) : Expr =
            let rawFreeVars = L.Expr.freeVars body

            let lamCtx = { ctx with locals = vars }

            let freeVars =
                Context.filterFreeVars lamCtx rawFreeVars

            let funCtx =
                { locals = []
                  globals = ctx.globals
                  env = List.append (Set.toList freeVars) ctx.env }

            let funName = buildFunction funCtx name vars body

            if freeVars.IsEmpty then
                Var(Var.Global funName)
            else
                let captured = (Set.toArray freeVars)
                Closure(funName, captured)

        and translateExpr (ctx: Context) (expr: L.Expr) : Expr =
            // TODO: when translating expressions I need to rename global names to their unique counterparts
            // Or.. in fact, all global names are synthetic and the actual binds are just mapped to generated functions
            // The renaming would rather happen at the constant prop pass where locals that reference global values are replaced.
            let go = translateExpr

            match expr with
            | L.Lit l -> Lit l
            | L.Var name ->
                // Can probably implement renaming at the context level
                Var(Context.makeVar ctx name)
            | L.Lam _ as lam ->
                let vars, body = L.Expr.collapseLambdas lam
                translateLam ctx "L" vars body
            | L.Bind (isRec, var, body, cont) ->
                let fnVars, fnBody = L.Expr.collapseLambdas body

                let bodyEnv =
                    if isRec then
                        var :: ctx.env
                    else
                        ctx.env

                let bodyCtx = { ctx with env = bodyEnv }

                let bodyT =
                    if fnVars.IsEmpty then
                        // Value binding: can be either a regular value or an expression returning a closure
                        go bodyCtx body
                    else
                        translateLam bodyCtx var fnVars fnBody

                let contCtx = { ctx with locals = var :: ctx.locals }
                let contT = go contCtx cont

                Bind
                    { isRec = isRec
                      name = var
                      body = bodyT
                      cont = contT }
            | L.App (expr, arg) ->
                let exprT = go ctx expr
                let argT = go ctx arg
                App(exprT, argT)
            | L.Cond (p, t, f) ->
                let pT = go ctx p
                let tT = go ctx t
                let fT = go ctx f
                Cond(pT, tT, fT)
            | L.Prim (L.BArith (fn, opA, opB)) ->
                let aT = go ctx opA
                let bT = go ctx opB
                Prim(BArith(fn, aT, bT))
            | L.Prim (L.UArith (fn, expr)) ->
                let exprT = go ctx expr
                Prim(UArith(fn, exprT))
            | L.Prim (L.Cmp (fn, lhs, rhs)) ->
                let lhsT = go ctx lhs
                let rhsT = go ctx rhs
                Prim(Cmp(fn, lhsT, rhsT))

        member this.Build() : Module =
            let expr = translateExpr Context.empty expr
            phrases.Add(Exec expr)
            phrases.ToArray()

    let translateExpr (expr: L.Expr) =
        let b = Builder(expr)
        b.Build()

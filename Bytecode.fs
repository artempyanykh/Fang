module Fang.Bytecode

open System.Collections.Generic
open Fang.Lang

[<RequireQualifiedAccess>]
type IntOp =
    | Add
    | Sub
    | Mul
    | Div

[<RequireQualifiedAccess>]
type IntCmp =
    | Less
    | Equal
    | Greater

type Const = Const of int

module Const =
    let unwrap (Const inner) : int = inner

type CodePointer = { label: int; offset: int }

type Instr =
    | Halt
    | Bottom
    | IntConst of int
    | IntOp of IntOp
    | IntCmp of IntCmp
    | EnvLoad of name: Const
    | EnvSave of name: Const
    | EnvDrop of name: Const
    | EnvSaveRec of name: Const
    | Jump of CodePointer
    | Branch of trueBranch: CodePointer * falseBranch: CodePointer
    | MakeClosure of var: Const * code: CodePointer
    | Apply
    | Return

type ConstPool() =
    let mapping: Dictionary<string, Const> = Dictionary()
    let reverseMapping: ResizeArray<string> = ResizeArray()

    member this.Add(value: string) : Const =
        if mapping.ContainsKey(value) then
            mapping.[value]
        else
            let nextNum = Const mapping.Count
            mapping.Add(value, nextNum)
            reverseMapping.Add(value)
            nextNum

    member this.FindNum(value: string) : Const = mapping.[value]

    member this.FindName(num: Const) : string = reverseMapping.[num |> Const.unwrap]

type Label = string

type LabelManager() =
    let mapping: Dictionary<Label, int> =
        Dictionary(Map.ofList [ (LabelManager.EntryLabel, 0) ])

    let reverseMapping: ResizeArray<Label> = ResizeArray([ LabelManager.EntryLabel ])

    member private this.AddLabel(name: string) : Label * int =
        let nextId = mapping.Count
        let label = $"l{nextId}_{name}"
        mapping.Add(label, nextId)
        reverseMapping.Add(label)
        label, nextId

    member this.GenLabel() : Label * int = this.AddLabel("gen")

    member this.GenNamedLabel(name: string) : Label * int = this.AddLabel(name)

    member this.GenScopedLabel(scope: Label, name: string) : Label * int =
        if scope = LabelManager.EntryLabel then
            this.AddLabel(name)
        else
            this.AddLabel($"{scope}_{name}")

    member this.FindLabelId(label: Label) : int = mapping.[label]
    member this.FindLabel(id: int) : Label = reverseMapping.[id]

    static member EntryLabel: Label = "entry"

type Bytecode() =
    let chunks: ResizeArray<ResizeArray<Instr>> = ResizeArray()
    member val ConstPool: ConstPool = ConstPool()
    member val LabelManager: LabelManager = LabelManager()

    member this.EmitInstr(label: Label, instr: Instr) : unit =
        let chunkNum = this.LabelManager.FindLabelId(label)

        while chunks.Count <= chunkNum do
            chunks.Add(ResizeArray())

        let cur = chunks.[chunkNum]
        cur.Add(instr)

    member this.GetInstructions(label: Label) : List<Instr> =
        chunks.[this.LabelManager.FindLabelId(label)]

    member this.GetInstructions(labelId: int) : List<Instr> = chunks.[labelId]

    member this.GetCurrentOffset(label: Label) : int = this.GetInstructions(label).Count


let rec genBytecodeImpl (bc: Bytecode) (initLabel: Label) (expr: Expr) : unit =
    match expr with
    | Lit (BType.Int v) -> bc.EmitInstr(initLabel, IntConst v)
    | Lit BType.Unit -> bc.EmitInstr(initLabel, IntConst 0)
    | Var (VarName var) ->
        let constNum = bc.ConstPool.Add var
        bc.EmitInstr(initLabel, EnvLoad constNum)
    | Lam (VarName var, body) ->
        let lamBodyLabel, lamBodyLabelNum =
            bc.LabelManager.GenScopedLabel(initLabel, "lam")

        genBytecodeImpl bc lamBodyLabel body
        bc.EmitInstr(lamBodyLabel, Return)

        let varNum = bc.ConstPool.Add var
        bc.EmitInstr(initLabel, MakeClosure(varNum, { label = lamBodyLabelNum; offset = 0 }))
    | App (expr, arg) ->
        genBytecodeImpl bc initLabel arg
        genBytecodeImpl bc initLabel expr
        bc.EmitInstr(initLabel, Apply)
    | Cond (p, t, f) ->
        genBytecodeImpl bc initLabel p

        let tbl, tblNum =
            bc.LabelManager.GenScopedLabel(initLabel, "tb")

        let fbl, fblNum =
            bc.LabelManager.GenScopedLabel(initLabel, "fbl")

        bc.EmitInstr(initLabel, Branch({ label = tblNum; offset = 0 }, { label = fblNum; offset = 0 }))

        let contPointer =
            { label = bc.LabelManager.FindLabelId(initLabel)
              offset = bc.GetCurrentOffset(initLabel) }

        genBytecodeImpl bc tbl t
        bc.EmitInstr(tbl, Jump contPointer)

        genBytecodeImpl bc fbl f
        bc.EmitInstr(fbl, Jump contPointer)
    | Builtin (Arithmetic (arithmeticFn, opA, opB)) ->
        genBytecodeImpl bc initLabel opA
        genBytecodeImpl bc initLabel opB

        match arithmeticFn with
        | Add -> bc.EmitInstr(initLabel, IntOp IntOp.Add)
        | Sub -> bc.EmitInstr(initLabel, IntOp IntOp.Sub)
        | Mul -> bc.EmitInstr(initLabel, IntOp IntOp.Mul)
        | Div -> bc.EmitInstr(initLabel, IntOp IntOp.Div)
    | Builtin (Comparison (comparisonFn, lhs, rhs)) ->
        genBytecodeImpl bc initLabel lhs
        genBytecodeImpl bc initLabel rhs

        match comparisonFn with
        | Less -> bc.EmitInstr(initLabel, IntCmp IntCmp.Less)
        | Equal -> bc.EmitInstr(initLabel, IntCmp IntCmp.Equal)
        | Greater -> bc.EmitInstr(initLabel, IntCmp IntCmp.Greater)
    | Bind (recursive, VarName var, body, expr) ->
        let varConst = bc.ConstPool.Add var

        if recursive then
            bc.EmitInstr(initLabel, Bottom)
            bc.EmitInstr(initLabel, EnvSave varConst)

            genBytecodeImpl bc initLabel body
            bc.EmitInstr(initLabel, EnvDrop varConst)
            bc.EmitInstr(initLabel, EnvSaveRec varConst)

            genBytecodeImpl bc initLabel expr
            bc.EmitInstr(initLabel, EnvDrop varConst)
        else
            genBytecodeImpl bc initLabel body
            bc.EmitInstr(initLabel, EnvSave varConst)
            genBytecodeImpl bc initLabel expr
            bc.EmitInstr(initLabel, EnvDrop varConst)

let genBytecode (expr: Expr) : Bytecode =
    let bc = Bytecode()
    genBytecodeImpl bc LabelManager.EntryLabel expr
    bc.EmitInstr(LabelManager.EntryLabel, Halt)
    bc

module VM =

    [<RequireQualifiedAccess>]
    type Value =
        | Bottom
        | Int of int
        | Closure of Closure
        | ClosureRec of Closure * recName: Const

    and Closure =
        { env: Env
          var: Const
          code: CodePointer }

    and Env =
        { cur: Map<Const, Value>
          prev: Option<Env> }

    module Env =
        let empty = { cur = Map.empty; prev = None }

        let tryFind (name: Const) (env: Env) : Option<Value> = Map.tryFind name env.cur

        let withBinding (name: Const) (value: Value) (env: Env) : Env =
            let cur = Map.add name value env.cur
            { cur = cur; prev = Some env }

    type InterpError =
        | WrongType of Value * expected: string
        | UnboundVar of name: string
        | AccessUneval of name: string
        | Internal of msg: string

    exception InterpException of InterpError

    let valueAsIntExn =
        function
        | Value.Int i -> i
        | other -> raise (InterpException(WrongType(other, "int")))

    let valueAsClosureExn =
        function
        | Value.Closure c -> c
        | other -> raise (InterpException(WrongType(other, "closure")))

    type VM(bc: Bytecode) =
        let stack: Stack<Value> = Stack()

        let mutable env: Env = Env.empty

        //        let mutable code: ResizeArray<Instr> = ResizeArray()
        let mutable labelId = 0
        let mutable instrIdx = 0

        let returnStack: Stack<int * int * Env> = Stack()

        member private this.RunLoop() =
            let mutable shouldHalt = false

            while (instrIdx < bc.GetInstructions(labelId).Count)
                  && (not shouldHalt) do
                let instr = bc.GetInstructions(labelId).[instrIdx]
                instrIdx <- instrIdx + 1

                match instr with
                | Halt -> shouldHalt <- true
                | Bottom -> stack.Push(Value.Bottom)
                | IntConst i -> stack.Push(Value.Int i)
                | IntOp op ->
                    let arg2 = stack.Pop() |> valueAsIntExn
                    let arg1 = stack.Pop() |> valueAsIntExn

                    let result =
                        match op with
                        | IntOp.Add -> arg1 + arg2
                        | IntOp.Sub -> arg1 - arg2
                        | IntOp.Mul -> arg1 * arg2
                        | IntOp.Div -> arg1 / arg2

                    stack.Push(Value.Int result)
                | IntCmp op ->
                    let arg2 = stack.Pop() |> valueAsIntExn
                    let arg1 = stack.Pop() |> valueAsIntExn

                    let result =
                        match op with
                        | IntCmp.Less -> if arg1 < arg2 then 1 else 0
                        | IntCmp.Equal -> if arg1 = arg2 then 1 else 0
                        | IntCmp.Greater -> if arg1 > arg2 then 1 else 0

                    stack.Push(Value.Int result)
                | EnvLoad name ->
                    match Env.tryFind name env with
                    | Some v ->
                        match v with
                        | Value.Bottom -> raise (InterpException(AccessUneval(bc.ConstPool.FindName(name))))
                        | _ -> stack.Push(v)
                    | None -> raise (InterpException(UnboundVar(bc.ConstPool.FindName(name))))
                | EnvSave name ->
                    let value = stack.Pop()
                    env <- Env.withBinding name value env
                | EnvDrop nameId ->
                    match env.prev with
                    | Some prev -> env <- prev
                    | _ ->
                        raise (
                            InterpException(
                                Internal $"Env violation: drop {bc.ConstPool.FindName(nameId)} with empty previous env"
                            )
                        )
                | EnvSaveRec nameId ->
                    // TODO: need to handle ClosureRec too?
                    let closureValue = stack.Pop() |> valueAsClosureExn
                    let closureRec = Value.ClosureRec(closureValue, nameId)
                    env <- Env.withBinding nameId closureRec env
                | Jump codePointer ->
                    labelId <- codePointer.label
                    instrIdx <- codePointer.offset
                | Branch (t, f) ->
                    let value = stack.Pop() |> valueAsIntExn
                    let target = if value <> 0 then t else f

                    labelId <- target.label
                    instrIdx <- target.offset
                | MakeClosure (nameId, labelId) ->
                    let closure =
                        Value.Closure
                            { env = env
                              var = nameId
                              code = labelId }

                    stack.Push(closure)
                | Apply ->
                    let potentialClosure = stack.Pop()
                    let arg = stack.Pop()

                    let newEnv, newCodePointer =
                        match potentialClosure with
                        | Value.Closure closure -> Env.withBinding closure.var arg closure.env, closure.code
                        | Value.ClosureRec (closure, recNameId) ->
                            let newEnv =
                                closure.env
                                |> Env.withBinding closure.var arg
                                |> Env.withBinding recNameId potentialClosure

                            newEnv, closure.code
                        | other -> raise (InterpException(WrongType(other, "closure")))

                    returnStack.Push(labelId, instrIdx, env)

                    labelId <- newCodePointer.label
                    instrIdx <- newCodePointer.offset
                    env <- newEnv
                | Return ->
                    let contLabelId, contInstrIdx, contEnv = returnStack.Pop()
                    labelId <- contLabelId
                    instrIdx <- contInstrIdx
                    env <- contEnv

        member this.Execute() =
            let mainLabelId =
                bc.LabelManager.FindLabelId(LabelManager.EntryLabel)

            labelId <- mainLabelId
            instrIdx <- 0

            this.RunLoop()

        member this.CurrentValue: Option<Value> =
            if stack.Count = 0 then
                None
            else
                Some(stack.Peek())

open VM

module Ex =
    let eval expr =
        let startTs = System.DateTime.Now

        let bc = genBytecode expr
        let bcDoneTs = System.DateTime.Now

        let vm = VM.VM(bc)

        try
            vm.Execute()
            let finishTs = System.DateTime.Now
            let value = vm.CurrentValue

            let bcDur = bcDoneTs - startTs
            let exeDur = finishTs - bcDoneTs
            printfn $"[{bcDur.TotalMilliseconds}/{exeDur.TotalMilliseconds}>> {value}"
        with
        | InterpException err -> printfn $"!! {err}"

    let ex1_AST =
        // 2 + 3 * 4
        Builtin(
            Arithmetic( //
                Add,
                Lit(BType.Int 2),
                Builtin(
                    Arithmetic( //
                        Mul,
                        Lit(BType.Int 3),
                        Lit(BType.Int 4)
                    )
                )
            )
        )

    let ex2_AST =
        // let x = 2 + 3 * 4 in let y = 42 in x - y
        Bind(
            recursive = false,
            var = VarName "x",
            body = ex1_AST,
            expr =
                Bind(
                    recursive = false,
                    var = VarName "y",
                    body = Lit(BType.Int 42),
                    expr = Builtin(Arithmetic(Sub, Var(VarName "x"), Var(VarName "y")))
                )
        )

    let ex3_AST =
        Bind(
            recursive = false,
            var = VarName "plus42",
            body = Lam(VarName "x", Builtin(Arithmetic(Add, Var(VarName "x"), Lit(BType.Int 42)))),
            expr = App(Var(VarName "plus42"), Lit(BType.Int 3))
        )

    let ex4_AST =
        Bind(
            recursive = false,
            var = VarName "plusN",
            body = Lam(VarName "N", Lam(VarName "x", Builtin(Arithmetic(Add, Var(VarName "x"), Var(VarName "N"))))),
            expr = App(App(Var(VarName "plusN"), Lit(BType.Int 3)), Lit(BType.Int 5))
        )


    let ex5_AST (num: int) =
        let fib = VarName "fib"
        let n = VarName "n"
        let one = Lit(BType.Int 1)
        let two = Lit(BType.Int 2)
        let leqOne = Builtin(Comparison(Less, Var n, two))

        let fn1 =
            App(Var fib, Builtin(Arithmetic(Sub, Var n, one)))

        let fn2 =
            App(Var fib, Builtin(Arithmetic(Sub, Var n, two)))

        Bind(
            recursive = true,
            var = fib,
            body = Lam(n, Cond(leqOne, one, Builtin(Arithmetic(Add, fn1, fn2)))),
            expr = App(Var fib, Lit(BType.Int num))
        )

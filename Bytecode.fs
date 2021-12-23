module Fang.Bytecode

open System.Collections.Generic
open Fang
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

type ConstNum = ConstNum of int

module ConstNum =
    let unwrap (ConstNum inner) : int = inner


type Label = string
type LabelNum = int

type CodePointer = { chunk: int; offset: int }

type Instr =
    | Halt
    | Bottom
    | IntConst of int
    | IntOp of IntOp
    | IntCmp of IntCmp
    | EnvLoad of name: ConstNum
    | EnvSave of name: ConstNum
    | EnvDrop of name: ConstNum
    | EnvSaveRec of name: ConstNum
    | Jump of LabelNum
    | Branch of trueBranch: LabelNum * falseBranch: LabelNum
    | MakeClosure of var: ConstNum * code: LabelNum
    | Apply
    | Return

type ConstPool() =
    let mapping: Dictionary<string, ConstNum> = Dictionary()
    let reverseMapping: ResizeArray<string> = ResizeArray()

    member this.Add(value: string) : ConstNum =
        if mapping.ContainsKey(value) then
            mapping.[value]
        else
            let nextNum = ConstNum mapping.Count
            mapping.Add(value, nextNum)
            reverseMapping.Add(value)
            nextNum

    member this.FindNum(value: string) : ConstNum = mapping.[value]

    member this.FindName(num: ConstNum) : string = reverseMapping.[num |> ConstNum.unwrap]


type LabelManager() =
    let mapping: Dictionary<Label, LabelNum> = Dictionary()

    let reverseMapping: ResizeArray<Label> = ResizeArray()

    member private this.AddLabel(name: string) : LabelNum =
        let nextId = mapping.Count

        let label =
            if name = LabelManager.EntryLabel then
                name
            else
                $"{name}{nextId}"

        mapping.Add(label, nextId)
        reverseMapping.Add(label)
        nextId

    member this.GenNamedLabel(name: string) : LabelNum = this.AddLabel(name)

    member this.FindLabelNum(label: Label) : LabelNum = mapping.[label]
    member this.FindLabel(num: LabelNum) : Label = reverseMapping.[num]

    static member EntryLabel: Label = "entry"

type Chunk = int

exception BytecodeException of string

type Bytecode() =
    let chunks: ResizeArray<ResizeArray<Instr>> = ResizeArray()
    let labelMapping: Dictionary<LabelNum, CodePointer> = Dictionary()

    member val ConstPool: ConstPool = ConstPool()
    member val LabelManager: LabelManager = LabelManager()

    member this.AllocChunk() : Chunk =
        let chunk = chunks.Count
        chunks.Add(ResizeArray())
        chunk

    member this.EmitInstr(chunk: Chunk, instr: Instr) : unit =
        let cur = chunks.[chunk]
        cur.Add(instr)

    member this.BindLabel(labelNum: LabelNum, cp: CodePointer) : unit = labelMapping.Add(labelNum, cp)

    member this.AllocNamedChunk(name: string) : Chunk * LabelNum =
        let chunk = this.AllocChunk()
        let labelNum = this.LabelManager.GenNamedLabel(name)
        this.BindLabel(labelNum, { chunk = chunk; offset = 0 })
        chunk, labelNum

    member this.GetInstructions(label: Label) : ResizeArray<Instr> * CodePointer =
        this.GetInstructions(this.LabelManager.FindLabelNum(label))

    member this.GetChunk(chunk: Chunk) : ResizeArray<Instr> = chunks.[chunk]

    member this.GetInstructions(labelNum: LabelNum) : ResizeArray<Instr> * CodePointer =
        let contains, cp = labelMapping.TryGetValue(labelNum)

        if not contains then
            raise (BytecodeException $"Unbound label: {this.LabelManager.FindLabel(labelNum)}")

        chunks.[cp.chunk], cp

    member this.getCodePointer(labelNum: LabelNum) : CodePointer = labelMapping.[labelNum]

    member this.GetCurrentOffset(chunk: Chunk) : int = chunks.[chunk].Count


let rec genBytecodeImpl (bc: Bytecode) (chunk: Chunk) (expr: Expr) : unit =
    match expr with
    | Lit (BType.Int v) -> bc.EmitInstr(chunk, IntConst v)
    | Lit BType.Unit -> bc.EmitInstr(chunk, IntConst 0)
    | Var (VarName var) ->
        let constNum = bc.ConstPool.Add var
        bc.EmitInstr(chunk, EnvLoad constNum)
    | Lam (VarName var, body) ->
        let lamBodyChunk, lamBodyLabelNum = bc.AllocNamedChunk("l")

        genBytecodeImpl bc lamBodyChunk body
        bc.EmitInstr(lamBodyChunk, Return)

        let varNum = bc.ConstPool.Add var
        bc.EmitInstr(chunk, MakeClosure(varNum, lamBodyLabelNum))
    | App (expr, arg) ->
        genBytecodeImpl bc chunk arg
        genBytecodeImpl bc chunk expr
        bc.EmitInstr(chunk, Apply)
    | Cond (p, t, f) ->
        genBytecodeImpl bc chunk p

        let tbChunk, tbLabelNum = bc.AllocNamedChunk("bt")

        let fbChunk, fbLabelNum = bc.AllocNamedChunk("bf")

        bc.EmitInstr(chunk, Branch(tbLabelNum, fbLabelNum))

        let contPointer =
            { chunk = chunk
              offset = bc.GetCurrentOffset(chunk) }

        let contLabel = bc.LabelManager.GenNamedLabel("bcont")
        bc.BindLabel(contLabel, contPointer)

        genBytecodeImpl bc tbChunk t
        bc.EmitInstr(tbChunk, Jump contLabel)

        genBytecodeImpl bc fbChunk f
        bc.EmitInstr(fbChunk, Jump contLabel)
    | Builtin (Arithmetic (arithmeticFn, opA, opB)) ->
        genBytecodeImpl bc chunk opA
        genBytecodeImpl bc chunk opB

        match arithmeticFn with
        | Add -> bc.EmitInstr(chunk, IntOp IntOp.Add)
        | Sub -> bc.EmitInstr(chunk, IntOp IntOp.Sub)
        | Mul -> bc.EmitInstr(chunk, IntOp IntOp.Mul)
        | Div -> bc.EmitInstr(chunk, IntOp IntOp.Div)
    | Builtin (Comparison (comparisonFn, lhs, rhs)) ->
        genBytecodeImpl bc chunk lhs
        genBytecodeImpl bc chunk rhs

        match comparisonFn with
        | Less -> bc.EmitInstr(chunk, IntCmp IntCmp.Less)
        | Equal -> bc.EmitInstr(chunk, IntCmp IntCmp.Equal)
        | Greater -> bc.EmitInstr(chunk, IntCmp IntCmp.Greater)
    | Bind (recursive, VarName var, body, expr) ->
        let varConst = bc.ConstPool.Add var

        if recursive then
            bc.EmitInstr(chunk, Bottom)
            bc.EmitInstr(chunk, EnvSave varConst)

            genBytecodeImpl bc chunk body
            bc.EmitInstr(chunk, EnvDrop varConst)
            bc.EmitInstr(chunk, EnvSaveRec varConst)

            genBytecodeImpl bc chunk expr
            bc.EmitInstr(chunk, EnvDrop varConst)
        else
            genBytecodeImpl bc chunk body
            bc.EmitInstr(chunk, EnvSave varConst)
            genBytecodeImpl bc chunk expr
            bc.EmitInstr(chunk, EnvDrop varConst)

let genBytecode (expr: Expr) : Bytecode =
    let bc = Bytecode()

    let entry, _ =
        bc.AllocNamedChunk(LabelManager.EntryLabel)

    genBytecodeImpl bc entry expr
    bc.EmitInstr(entry, Halt)
    bc

module VM =

    [<RequireQualifiedAccess>]
    type Value =
        | Bottom
        | Int of int
        | Closure of Closure
        | ClosureRec of Closure * recName: ConstNum

    and Closure =
        { env: Env
          var: ConstNum
          code: CodePointer }

    and Env =
        { cur: Map<ConstNum, Value>
          prev: Option<Env> }

    module Env =
        let empty = { cur = Map.empty; prev = None }

        let tryFind (name: ConstNum) (env: Env) : Option<Value> = Map.tryFind name env.cur

        let withBinding (name: ConstNum) (value: Value) (env: Env) : Env =
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

        let mutable code: ResizeArray<Instr> = ResizeArray()
        let mutable codePointer: CodePointer = { chunk = 0; offset = 0 }

        let returnStack: Stack<CodePointer * Env> = Stack()

        member private this.RunLoop() =
            let mutable shouldHalt = false

            while (codePointer.offset < code.Count)
                  && (not shouldHalt) do
                let instr = code.[codePointer.offset]

                codePointer <-
                    { codePointer with
                          offset = codePointer.offset + 1 }

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
                | Jump label ->
                    let newCode, newCodePointer = bc.GetInstructions(label)
                    code <- newCode
                    codePointer <- newCodePointer
                | Branch (t, f) ->
                    let value = stack.Pop() |> valueAsIntExn
                    let target = if value <> 0 then t else f

                    let newCode, newCodePointer = bc.GetInstructions(target)
                    code <- newCode
                    codePointer <- newCodePointer
                | MakeClosure (var, label) ->
                    let code = bc.getCodePointer label

                    let closure =
                        Value.Closure { env = env; var = var; code = code }

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

                    returnStack.Push(codePointer, env)

                    code <- bc.GetChunk(newCodePointer.chunk)
                    codePointer <- newCodePointer
                    env <- newEnv
                | Return ->
                    let contCodePointer, contEnv = returnStack.Pop()
                    code <- bc.GetChunk(contCodePointer.chunk)
                    codePointer <- contCodePointer
                    env <- contEnv

        member this.Execute() =
            let entryCode, entryCodePointer =
                bc.GetInstructions(LabelManager.EntryLabel)

            code <- entryCode
            codePointer <- entryCodePointer

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

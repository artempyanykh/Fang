module Fang.ChunkedBytecode

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

[<Struct>]
type ConstNum = ConstNum of int

module ConstNum =
    let unwrap (ConstNum inner) : int = inner


type Label = string
type LabelNum = int

[<Struct>]
type CodePointer = { chunk: int; offset: int }

type Instr =
    | Halt
    | Nop
    | Bottom
    | IntConst of int
    | IntOperation of IntOp
    | IntComparison of IntCmp
    | EnvLoad of name: ConstNum
    | EnvSave of name: ConstNum
    | EnvDrop of name: ConstNum
    | EnvSaveRec of name: ConstNum
    | Jump of LabelNum
    | BranchTrue of LabelNum
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

    member this.GetChunks() : ResizeArray<ResizeArray<Instr>> = chunks

    member this.GetInstructions(labelNum: LabelNum) : ResizeArray<Instr> * CodePointer =
        let contains, cp = labelMapping.TryGetValue(labelNum)

        if not contains then
            raise (BytecodeException $"Unbound label: {this.LabelManager.FindLabel(labelNum)}")

        chunks.[cp.chunk], cp

    member this.GetCodePointer(labelNum: LabelNum) : CodePointer = labelMapping.[labelNum]

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
        let contLabel = bc.LabelManager.GenNamedLabel("br_cont")
        let tbLabel = bc.LabelManager.GenNamedLabel("br_tb")

        bc.EmitInstr(chunk, BranchTrue tbLabel)

        // Generate false branch first
        genBytecodeImpl bc chunk f
        // Fall-through to contLabel when done with the false branch
        bc.EmitInstr(chunk, Jump contLabel)

        // Bind tbLabel and generate the true branch code
        let tbPointer =
            { chunk = chunk
              offset = bc.GetCurrentOffset(chunk) }

        bc.BindLabel(tbLabel, tbPointer)
        genBytecodeImpl bc chunk t

        // Bind the contLabel:
        // * we jump to it from false branch,
        // * there's a fall-through to it from true branch
        let contPointer =
            { chunk = chunk
              offset = bc.GetCurrentOffset(chunk) }

        bc.BindLabel(contLabel, contPointer)
        bc.EmitInstr(chunk, Nop)
    | Builtin (Arithmetic (arithmeticFn, opA, opB)) ->
        genBytecodeImpl bc chunk opA
        genBytecodeImpl bc chunk opB

        match arithmeticFn with
        | Add -> bc.EmitInstr(chunk, IntOperation IntOp.Add)
        | Sub -> bc.EmitInstr(chunk, IntOperation IntOp.Sub)
        | Mul -> bc.EmitInstr(chunk, IntOperation IntOp.Mul)
        | Div -> bc.EmitInstr(chunk, IntOperation IntOp.Div)
    | Builtin (Comparison (comparisonFn, lhs, rhs)) ->
        genBytecodeImpl bc chunk lhs
        genBytecodeImpl bc chunk rhs

        match comparisonFn with
        | Less -> bc.EmitInstr(chunk, IntComparison IntCmp.Less)
        | Equal -> bc.EmitInstr(chunk, IntComparison IntCmp.Equal)
        | Greater -> bc.EmitInstr(chunk, IntComparison IntCmp.Greater)
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

module ChunkedVM =

    [<RequireQualifiedAccess>]
    [<Struct>]
    type Value =
        | Bottom
        | Int of intVal: int
        | Closure of regularClosure: Closure
        | ClosureRec of recClosure: Closure * recVar: ConstNum

    and Closure =
        { env: Env
          var: ConstNum
          code: CodePointer }

    and Env = Map<ConstNum, Value>

    type InterpError =
        | WrongType of Value * expected: string
        | UnboundVar of name: string
        | AccessUneval of name: string
        | Internal of msg: string

    exception InterpException of InterpError

    module Env =
        let empty = Map.empty

        let find (name: ConstNum) (env: Env) : Value =
            try
                Map.find name env
            with
            | :? KeyNotFoundException -> raise (InterpException(UnboundVar($"#{name}")))

        let withBinding (name: ConstNum) (value: Value) (env: Env) : Env = Map.add name value env


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
        let mutable envStack: Stack<Env> = Stack()

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
                | Nop -> ()
                | Bottom -> stack.Push(Value.Bottom)
                | IntConst i -> stack.Push(Value.Int i)
                | IntOperation op ->
                    let arg2 = stack.Pop() |> valueAsIntExn
                    let arg1 = stack.Pop() |> valueAsIntExn

                    let result =
                        match op with
                        | IntOp.Add -> arg1 + arg2
                        | IntOp.Sub -> arg1 - arg2
                        | IntOp.Mul -> arg1 * arg2
                        | IntOp.Div -> arg1 / arg2

                    stack.Push(Value.Int result)
                | IntComparison op ->
                    let arg2 = stack.Pop() |> valueAsIntExn
                    let arg1 = stack.Pop() |> valueAsIntExn

                    let result =
                        match op with
                        | IntCmp.Less -> if arg1 < arg2 then 1 else 0
                        | IntCmp.Equal -> if arg1 = arg2 then 1 else 0
                        | IntCmp.Greater -> if arg1 > arg2 then 1 else 0

                    stack.Push(Value.Int result)
                | EnvLoad name ->
                    match Env.find name env with
                    | Value.Bottom -> raise (InterpException(AccessUneval(bc.ConstPool.FindName(name))))
                    | v -> stack.Push(v)
                | EnvSave name ->
                    let value = stack.Pop()
                    envStack.Push(env)
                    env <- Env.withBinding name value env
                | EnvDrop nameId ->
                    env <- envStack.Pop()
                | EnvSaveRec nameId ->
                    // TODO: need to handle ClosureRec too?
                    let closureValue = stack.Pop() |> valueAsClosureExn
                    let closureRec = Value.ClosureRec(closureValue, nameId)
                    envStack.Push(env)
                    env <- Env.withBinding nameId closureRec env
                | Jump label ->
                    let newCode, newCodePointer = bc.GetInstructions(label)
                    code <- newCode
                    codePointer <- newCodePointer
                | BranchTrue target ->
                    let value = stack.Pop() |> valueAsIntExn

                    if value <> 0 then
                        let newCode, newCodePointer = bc.GetInstructions(target)
                        code <- newCode
                        codePointer <- newCodePointer
                | MakeClosure (var, label) ->
                    let code = bc.GetCodePointer label

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

module Ex =
    open ChunkedVM

    let evalPrint expr =
        let startTs = System.DateTime.Now

        let bc = genBytecode expr
        let bcDoneTs = System.DateTime.Now

        let vm = ChunkedVM.VM(bc)

        try
            vm.Execute()
            let finishTs = System.DateTime.Now
            let value = vm.CurrentValue

            let bcDur = bcDoneTs - startTs
            let exeDur = finishTs - bcDoneTs
            printfn $"[{bcDur.TotalMilliseconds}/{exeDur.TotalMilliseconds}ms]>> {value}"
        with
        | InterpException err -> printfn $"!! {err}"

module Fang.Bytecode

open System.Collections.Generic
open Fang.Lang

[<RequireQualifiedAccess>]
type IntBinaryOp =
    // Arithmetic
    | Add
    | Sub
    | Mul
    | Div
    // Comparison
    | Less
    | Equal
    | Greater

module IntBinaryOp =
    let fromArithmeticFn (fn: ArithmeticFn) =
        match fn with
        | Add -> IntBinaryOp.Add
        | Sub -> IntBinaryOp.Sub
        | Mul -> IntBinaryOp.Mul
        | Div -> IntBinaryOp.Div

    let fromComparisonFn (fn: ComparisonFn) =
        match fn with
        | Less -> IntBinaryOp.Less
        | Equal -> IntBinaryOp.Equal
        | Greater -> IntBinaryOp.Greater


[<RequireQualifiedAccess>]
type IntUnaryOp = | Neg

type ConstNum = int

type ConstPool = array<string>

module ConstPool =
    let findName (num: ConstNum) (pool: ConstPool) : string = pool.[num]

type ConstPoolBuilder() =
    let name2num: Dictionary<string, ConstNum> = Dictionary()

    member this.Add(name: string) : ConstNum =
        if name2num.ContainsKey(name) then
            name2num.[name]
        else
            let nextNum = name2num.Count
            name2num.Add(name, nextNum)
            nextNum

    member this.Build() : ConstPool =
        let num2name = Array.zeroCreate name2num.Count

        for kv in name2num do
            num2name.[kv.Value] <- kv.Key

        num2name

type LabelNum = int

type Labels =
    { offsets: array<int>
      names: array<int> }

type LabelManager() =
    let name2num: Dictionary<string, LabelNum> = Dictionary()

    member this.Add(label: string) : LabelNum =
        let nextNum = name2num.Count

        let uniqueName =
            if name2num.ContainsKey(label) then
                $"{label}{nextNum}"
            else
                label

        name2num.Add(uniqueName, nextNum)
        nextNum

    member this.Labels: seq<string * LabelNum> =
        seq {
            for kv in name2num do
                yield kv.Key, kv.Value
        }


[<RequireQualifiedAccess>]
type Instr =
    | Halt
    | IntConst of int
    | IntBinaryOp of IntBinaryOp
    | IntUnaryOp of IntUnaryOp
    | EnvLoad of ConstNum
    | EnvSave of ConstNum
    | EnvUpdate of ConstNum
    | EnvRestore of ConstNum
    | Blackhole
    | MakeClosure of var: ConstNum * code: LabelNum
    | Apply
    | Return
    | JumpTrue of LabelNum
    | Jump of LabelNum
    | Nop

type Bytecode =
    { instr: array<Instr>
      labels: Labels
      constPool: ConstPool }

type ChunkNum = int
type ChunkPointer = { chunk: ChunkNum; offset: int }

type BytecodeBuilder() =
    let constPool = ConstPoolBuilder()

    let labelManager = LabelManager()
    let labelMapping: Dictionary<LabelNum, ChunkPointer> = Dictionary()

    let bytecodeChunks: ResizeArray<ResizeArray<Instr>> = ResizeArray()

    member this.EmitInstr(chunk: ChunkNum, instr: Instr) = //
        bytecodeChunks.[chunk].Add(instr)

    member this.Generate(chunk: ChunkNum, expr: Expr) =
        match expr with
        | Lit (BType.Int i) -> this.EmitInstr(chunk, Instr.IntConst i)
        | Lit BType.Unit -> this.EmitInstr(chunk, Instr.IntConst 0)
        | Builtin (BuiltinFn.Arithmetic (fn, opA, opB)) ->
            this.Generate(chunk, opA) // stack: valueA
            this.Generate(chunk, opB) // stack: valueA valueB

            let opType = IntBinaryOp.fromArithmeticFn fn
            this.EmitInstr(chunk, Instr.IntBinaryOp opType)
        | Builtin (BuiltinFn.Comparison (fn, lhs, rhs)) ->
            this.Generate(chunk, lhs) // stack: valueL
            this.Generate(chunk, rhs) // stack: valueL valueR

            let opType = IntBinaryOp.fromComparisonFn fn
            this.EmitInstr(chunk, Instr.IntBinaryOp opType)
        | Builtin (BuiltinFn.UnaryArithmetic (fn, expr)) ->
            this.Generate(chunk, expr)

            match fn with
            | Neg -> this.EmitInstr(chunk, Instr.IntUnaryOp IntUnaryOp.Neg)
        | Var name ->
            let nameNum = constPool.Add(name)
            this.EmitInstr(chunk, Instr.EnvLoad nameNum)
        | Bind (recursive, var, body, expr) ->
            let varNum = constPool.Add(var)

            if not recursive then
                this.Generate(chunk, body)
                this.EmitInstr(chunk, Instr.EnvSave varNum)
                this.Generate(chunk, expr)
                this.EmitInstr(chunk, Instr.EnvRestore varNum)
            else
                this.EmitInstr(chunk, Instr.Blackhole)
                this.EmitInstr(chunk, Instr.EnvSave varNum)
                this.Generate(chunk, body)
                this.EmitInstr(chunk, Instr.EnvUpdate varNum)

                this.Generate(chunk, expr)
                this.EmitInstr(chunk, Instr.EnvRestore varNum)
        | Abs (var, body) ->
            // let a = 7 in
            // let x = 5 in
            // let f = (\x. x + a) in
            // 42 + (f x)
            //
            // IntConst 5
            // EnvSave 0
            // MakeClosure x label
            // 1: need several "chunks" of instructions
            // 2: lambda compilation should produce a value on the stack
            // 3: lambda value needs to reference the environment where it was defined
            let bodyChunk, bodyLabel = this.AllocNamedChunk("L")
            this.Generate(bodyChunk, body)
            this.EmitInstr(bodyChunk, Instr.Return)

            let varNum = constPool.Add(var)
            this.EmitInstr(chunk, Instr.MakeClosure(varNum, bodyLabel))
        | App (expr, arg) ->
            this.Generate(chunk, arg) // stack: argValue
            this.Generate(chunk, expr) // stack: argValue closureValue
            this.EmitInstr(chunk, Instr.Apply) // stack: applicationValue
        | Cond (pred, trueBranch, falseBranch) ->
            // Bytecode shape:
            //         pred_instr
            //         JumpTrue <TrBr>
            //         false_branch_instr
            //         JUMP <Cont>
            //   TrBr: true_branch
            //   Cont: NOP
            this.Generate(chunk, pred)
            let tbLabel = labelManager.Add("TB")
            let contLabel = labelManager.Add("CONT")
            this.EmitInstr(chunk, Instr.JumpTrue tbLabel)

            this.Generate(chunk, falseBranch)
            this.EmitInstr(chunk, Instr.Jump contLabel)

            this.BindLabel(
                tbLabel,
                { chunk = chunk
                  offset = bytecodeChunks.[chunk].Count }
            )

            this.Generate(chunk, trueBranch)

            this.BindLabel(
                contLabel,
                { chunk = chunk
                  offset = bytecodeChunks.[chunk].Count }
            )

            this.EmitInstr(chunk, Instr.Nop)

    member this.BindLabel(label: LabelNum, chunkPointer: ChunkPointer) = labelMapping.Add(label, chunkPointer)

    member this.AllocNamedChunk(name: string) : ChunkNum * LabelNum =
        let labelNum = labelManager.Add(name)
        let chunkNum = bytecodeChunks.Count
        labelMapping.Add(labelNum, { chunk = chunkNum; offset = 0 })
        bytecodeChunks.Add(ResizeArray())
        chunkNum, labelNum

    member this.Build() =
        let flatBytecode: ResizeArray<Instr> = ResizeArray()
        let chunkOffsets: ResizeArray<int> = ResizeArray()

        let mutable currentOffset = 0

        for chunk in bytecodeChunks do
            chunkOffsets.Add(currentOffset)
            flatBytecode.AddRange(chunk)
            currentOffset <- currentOffset + chunk.Count

        let labelOffsets = Array.zeroCreate labelMapping.Count
        let labelNames = Array.zeroCreate labelMapping.Count

        for labelName, labelNum in labelManager.Labels do
            let mapping = labelMapping.[labelNum]

            let chunkOffset =
                chunkOffsets.[mapping.chunk] + mapping.offset

            labelNames.[labelNum] <- constPool.Add(labelName)
            labelOffsets.[labelNum] <- chunkOffset

        let instr = Array.ofSeq flatBytecode
        let constPool = constPool.Build()

        let labels =
            { names = labelNames
              offsets = labelOffsets }

        { instr = instr
          labels = labels
          constPool = constPool }

let genBytecode (expr: Expr) : Bytecode =
    let bb = BytecodeBuilder()
    let entryChunk, _ = bb.AllocNamedChunk("entry")
    bb.Generate(entryChunk, expr)
    bb.EmitInstr(entryChunk, Instr.Halt)
    bb.Build()

module VM =
    [<RequireQualifiedAccess>]
    [<Struct>]
    type Value =
        | Int of intVal: int
        | Closure of closureVal: Closure
        | Blackhole

    and Env = Map<ConstNum, Value>

    and Closure =
        { mutable env: Env
          var: ConstNum
          codePointer: LabelNum }

    module Env =
        let empty = Map.empty

        let inline find (name: ConstNum) (env: Env) : Value =
            try
                Map.find name env
            with
            | :? KeyNotFoundException -> failwith $"Unbound name: #{name}"

    module Value =
        let asInt v =
            match v with
            | Value.Int i -> i
            | other -> failwith $"Expected int value: {other}"

        let asClosure v =
            match v with
            | Value.Closure c -> c
            | other -> failwith $"Expected closure value: {other}"

    type VM(bc: Bytecode) =
        let valueStack: Stack<Value> = Stack()

        let mutable env: Env = Env.empty
        let envStack: Stack<Env> = Stack()

        let returnStack: Stack<int> = Stack()

        let mutable codePointer: int = 0

        member this.Execute() =
            let mutable shouldHalt = false
            let instr = bc.instr
            let labelOffsets = bc.labels.offsets

            while (codePointer < instr.Length && not shouldHalt) do
                let curInstr = instr.[codePointer]
                codePointer <- codePointer + 1

                match curInstr with
                | Instr.Halt -> shouldHalt <- true
                | Instr.IntConst i -> valueStack.Push(Value.Int i)
                | Instr.IntBinaryOp intBinaryOp ->
                    let arg2 = valueStack.Pop() |> Value.asInt
                    let arg1 = valueStack.Pop() |> Value.asInt

                    let result =
                        match intBinaryOp with
                        | IntBinaryOp.Add -> arg1 + arg2
                        | IntBinaryOp.Sub -> arg1 - arg2
                        | IntBinaryOp.Mul -> arg1 * arg2
                        | IntBinaryOp.Div -> arg1 / arg2
                        | IntBinaryOp.Less -> if arg1 < arg2 then 1 else 0
                        | IntBinaryOp.Equal -> if arg1 = arg2 then 1 else 0
                        | IntBinaryOp.Greater -> if arg1 > arg2 then 1 else 0

                    valueStack.Push(Value.Int result)
                | Instr.IntUnaryOp intUnaryOp ->
                    let arg = valueStack.Pop() |> Value.asInt

                    let result =
                        match intUnaryOp with
                        | IntUnaryOp.Neg -> -arg

                    valueStack.Push(Value.Int result)
                | Instr.EnvLoad n ->
                    let value = Env.find n env
                    valueStack.Push(value)
                | Instr.EnvSave n ->
                    let value = valueStack.Pop()
                    envStack.Push(env)

                    env <- Map.add n value env
                | Instr.EnvUpdate n ->
                    let value = valueStack.Pop()
                    env <- Map.add n value env

                    match value with
                    | Value.Closure c -> c.env <- env
                    | _ -> ()
                | Instr.EnvRestore _ -> env <- envStack.Pop()
                | Instr.Blackhole -> valueStack.Push(Value.Blackhole)
                | Instr.MakeClosure (var, code) ->
                    let closure =
                        Value.Closure
                            { env = env
                              var = var
                              codePointer = code }

                    valueStack.Push(closure)
                | Instr.Apply ->
                    let closure = valueStack.Pop() |> Value.asClosure
                    let arg = valueStack.Pop()

                    let closureEnv = Map.add closure.var arg closure.env
                    envStack.Push(env)
                    env <- closureEnv

                    returnStack.Push(codePointer)
                    codePointer <- labelOffsets.[closure.codePointer]
                | Instr.Return ->
                    env <- envStack.Pop()
                    codePointer <- returnStack.Pop()
                | Instr.JumpTrue label ->
                    let predValue = valueStack.Pop() |> Value.asInt

                    if predValue <> 0 then
                        codePointer <- labelOffsets.[label]
                | Instr.Jump label -> codePointer <- labelOffsets.[label]
                | Instr.Nop -> ()

        member this.CurrentValue() : Option<Value> =
            if valueStack.Count = 0 then
                None
            else
                Some(valueStack.Peek())

module Ex =
    let eval (expr: Expr) : VM.Value =
        let bc = genBytecode expr
        let vm = VM.VM(bc)
        vm.Execute()
        vm.CurrentValue() |> Option.get

    let evalPrint expr =
        let startTs = System.DateTime.Now

        let bc = genBytecode expr
        let bcDoneTs = System.DateTime.Now

        try
            let vm = VM.VM(bc)
            vm.Execute()
            let finishTs = System.DateTime.Now

            let value = vm.CurrentValue() |> Option.get

            let bcDur = bcDoneTs - startTs
            let exeDur = finishTs - bcDoneTs
            $"[{bcDur.TotalMilliseconds}/{exeDur.TotalMilliseconds}ms]>> {value}"
        with
        | err -> $"!! {err}"

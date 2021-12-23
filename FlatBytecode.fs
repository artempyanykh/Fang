module Fang.FlatBytecode

open System
open Fang.SymbolicBytecode

module SB = SymbolicBytecode

type Bytecode = { code: array<byte>; entry: int }

module OpCodeNum =
    [<Literal>]
    let Halt = 255uy

    [<Literal>]
    let Nop = 0uy

    [<Literal>]
    let Bottom = 254uy

    [<Literal>]
    let IntConst = 10uy

    [<Literal>]
    let IntAdd = 11uy

    [<Literal>]
    let IntSub = 12uy

    [<Literal>]
    let IntMul = 13uy

    [<Literal>]
    let IntDiv = 14uy

    [<Literal>]
    let IntLess = 15uy

    [<Literal>]
    let IntEqual = 16uy

    [<Literal>]
    let IntGreater = 17uy

    [<Literal>]
    let EnvLoad = 20uy

    [<Literal>]
    let EnvSave = 21uy

    [<Literal>]
    let EnvDrop = 22uy

    [<Literal>]
    let EnvSaveRec = 23uy

    [<Literal>]
    let Jump = 30uy

    [<Literal>]
    let BranchTrue = 31uy

    [<Literal>]
    let MakeClosure = 40uy

    [<Literal>]
    let Apply = 41uy

    [<Literal>]
    let Return = 42uy

type BytecodeElement =
    | OpCode of byte
    | ImmInt of int
    | JumpTarget of codePointer: int
    | UnresolvedJumpTarget of labelNum: int

module BytecodeElement =
    let fromInstr (i: SB.Instr) : List<BytecodeElement> =
        match i with
        | SB.Instr.Halt -> [ OpCode OpCodeNum.Halt ]
        | SB.Instr.Nop -> [ OpCode OpCodeNum.Nop ]
        | SB.Instr.Bottom -> [ OpCode OpCodeNum.Bottom ]
        | SB.Instr.IntConst v -> [ OpCode OpCodeNum.IntConst; ImmInt v ]
        | SB.Instr.IntOperation op ->
            let code =
                match op with
                | SB.IntOp.Add -> OpCodeNum.IntAdd
                | SB.IntOp.Sub -> OpCodeNum.IntSub
                | SB.IntOp.Mul -> OpCodeNum.IntMul
                | SB.IntOp.Div -> OpCodeNum.IntDiv

            [ OpCode code ]
        | SB.Instr.IntComparison cmp ->
            let code =
                match cmp with
                | SB.IntCmp.Less -> OpCodeNum.IntLess
                | SB.IntCmp.Equal -> OpCodeNum.IntEqual
                | SB.IntCmp.Greater -> OpCodeNum.IntGreater

            [ OpCode code ]
        | SB.Instr.EnvLoad (ConstNum i) -> [ OpCode OpCodeNum.EnvLoad; ImmInt i ]
        | SB.Instr.EnvSave (ConstNum i) -> [ OpCode OpCodeNum.EnvSave; ImmInt i ]
        | SB.Instr.EnvDrop (ConstNum i) -> [ OpCode OpCodeNum.EnvDrop; ImmInt i ]
        | SB.Instr.EnvSaveRec (ConstNum i) ->
            [ OpCode OpCodeNum.EnvSaveRec
              ImmInt i ]
        | SB.Instr.Jump target ->
            [ OpCode OpCodeNum.Jump
              UnresolvedJumpTarget target ]
        | SB.Instr.BranchTrue target ->
            [ OpCode OpCodeNum.BranchTrue
              UnresolvedJumpTarget target ]
        | SB.Instr.MakeClosure (ConstNum i, code) ->
            [ OpCode OpCodeNum.MakeClosure
              ImmInt i
              UnresolvedJumpTarget code ]
        | SB.Instr.Apply -> [ OpCode OpCodeNum.Apply ]
        | SB.Instr.Return -> [ OpCode OpCodeNum.Return ]


    let byteWidth =
        function
        | OpCode _ -> 1
        | ImmInt _ -> 4
        | JumpTarget _ -> 4
        | UnresolvedJumpTarget _ -> 4

    let serializeInt (num: int) : array<byte> =
        let bytes = BitConverter.GetBytes(num)

        if not BitConverter.IsLittleEndian then
            Array.Reverse(bytes)

        bytes

    let deserializeInt (source: array<byte>, start: int) : int =
        let bytes = source.[start..start + 3]

        if not BitConverter.IsLittleEndian then
            Array.Reverse(bytes)

        BitConverter.ToInt32(bytes)

    let bytes (el: BytecodeElement) : seq<byte> =
        match el with
        | OpCode c -> [ byte c ]
        | ImmInt i -> serializeInt (i)
        | JumpTarget codePointer -> serializeInt (codePointer)
        | UnresolvedJumpTarget labelNum -> failwith $"Unresolved jump target cannot be converted: {labelNum}"



type Dictionary<'K, 'V> = System.Collections.Generic.Dictionary<'K, 'V>

module Bytecode =
    let build (sb: SB.SymbolicBytecode) : Bytecode =
        let mutable curOffset = 0
        let offsetMap: Dictionary<int * int, int> = Dictionary()
        let bytecode: ResizeArray<byte> = ResizeArray()

        let chunks = sb.GetChunks()

        // Pass 1: generate elements with unresolved jump targets, calculate offsets
        let elements: ResizeArray<BytecodeElement> = ResizeArray()

        for chunkNum = 0 to chunks.Count - 1 do
            let chunk = chunks.[chunkNum]

            for instrNum = 0 to chunk.Count - 1 do
                let instr = chunk.[instrNum]

                offsetMap.Add((chunkNum, instrNum), curOffset)
                let instrEls = BytecodeElement.fromInstr instr
                elements.AddRange(instrEls)

                let elsWidth =
                    instrEls
                    |> List.map BytecodeElement.byteWidth
                    |> List.sum

                curOffset <- curOffset + elsWidth

        // Pass 2: replace unresolved jump targets with resolved ones
        for i = 0 to elements.Count - 1 do
            let el = elements.[i]

            match el with
            | UnresolvedJumpTarget labelNum ->
                let labelPointer = sb.GetCodePointer(labelNum)

                let labelOffset =
                    offsetMap.[(labelPointer.chunk, labelPointer.offset)]

                elements.[i] <- JumpTarget labelOffset
            | _ -> ()


        for el in elements do
            bytecode.AddRange(BytecodeElement.bytes el)

        let entryPointer =
            sb.LabelManager.FindLabelNum(SB.LabelManager.EntryLabel)
            |> sb.GetCodePointer

        let entry =
            offsetMap.[(entryPointer.chunk, entryPointer.offset)]

        { code = bytecode.ToArray()
          entry = entry }

module FlatVM =

    type ConstNum = int
    type CodePointer = int

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

    type Stack<'V> = System.Collections.Generic.Stack<'V>

    type VM(bc: Bytecode) =
        let stack: Stack<Value> = Stack()
        let mutable env: Env = Env.empty

        let mutable codePointer: CodePointer = bc.entry

        let returnStack: Stack<CodePointer * Env> = Stack()

        member private this.RunLoop() =
            let mutable shouldHalt = false

            while (codePointer < bc.code.Length) && (not shouldHalt) do
                let instr = bc.code.[codePointer]
                codePointer <- codePointer + 1

                match instr with
                | OpCodeNum.Halt -> shouldHalt <- true
                | OpCodeNum.Nop -> ()
                | OpCodeNum.Bottom -> stack.Push(Value.Bottom)
                | OpCodeNum.IntConst ->
                    let arg =
                        BytecodeElement.deserializeInt (bc.code, codePointer)

                    codePointer <- codePointer + 4
                    stack.Push(Value.Int arg)
                | OpCodeNum.IntAdd
                | OpCodeNum.IntSub
                | OpCodeNum.IntMul
                | OpCodeNum.IntDiv ->
                    let arg2 = stack.Pop() |> valueAsIntExn
                    let arg1 = stack.Pop() |> valueAsIntExn

                    let result =
                        match instr with
                        | OpCodeNum.IntAdd -> arg1 + arg2
                        | OpCodeNum.IntSub -> arg1 - arg2
                        | OpCodeNum.IntMul -> arg1 * arg2
                        | OpCodeNum.IntDiv -> arg1 / arg2
                        | _ -> failwith "Impossible"

                    stack.Push(Value.Int result)
                | OpCodeNum.IntLess
                | OpCodeNum.IntEqual
                | OpCodeNum.IntGreater ->
                    let arg2 = stack.Pop() |> valueAsIntExn
                    let arg1 = stack.Pop() |> valueAsIntExn

                    let result =
                        match instr with
                        | OpCodeNum.IntLess -> if arg1 < arg2 then 1 else 0
                        | OpCodeNum.IntEqual -> if arg1 = arg2 then 1 else 0
                        | OpCodeNum.IntGreater -> if arg1 > arg2 then 1 else 0
                        | _ -> failwith "Impossible"

                    stack.Push(Value.Int result)
                | OpCodeNum.EnvLoad ->
                    let name =
                        BytecodeElement.deserializeInt (bc.code, codePointer)

                    codePointer <- codePointer + 4

                    match Env.tryFind name env with
                    | Some v ->
                        match v with
                        | Value.Bottom -> raise (InterpException(AccessUneval($"#{name}")))
                        | _ -> stack.Push(v)
                    | None -> raise (InterpException(UnboundVar($"#{name}")))
                | OpCodeNum.EnvSave ->
                    let name =
                        BytecodeElement.deserializeInt (bc.code, codePointer)

                    codePointer <- codePointer + 4

                    let value = stack.Pop()
                    env <- Env.withBinding name value env
                | OpCodeNum.EnvDrop ->
                    let name =
                        BytecodeElement.deserializeInt (bc.code, codePointer)

                    codePointer <- codePointer + 4

                    match env.prev with
                    | Some prev -> env <- prev
                    | _ -> raise (InterpException(Internal $"Env violation: drop #{name} with empty previous env"))
                | OpCodeNum.EnvSaveRec ->
                    let name =
                        BytecodeElement.deserializeInt (bc.code, codePointer)

                    codePointer <- codePointer + 4

                    // TODO: need to handle ClosureRec too?
                    let closureValue = stack.Pop() |> valueAsClosureExn
                    let closureRec = Value.ClosureRec(closureValue, name)
                    env <- Env.withBinding name closureRec env
                | OpCodeNum.Jump ->
                    let target =
                        BytecodeElement.deserializeInt (bc.code, codePointer)

                    codePointer <- target
                | OpCodeNum.BranchTrue ->
                    let target =
                        BytecodeElement.deserializeInt (bc.code, codePointer)

                    let value = stack.Pop() |> valueAsIntExn

                    if value <> 0 then
                        codePointer <- target
                    else
                        codePointer <- codePointer + 4
                | OpCodeNum.MakeClosure ->
                    let var =
                        BytecodeElement.deserializeInt (bc.code, codePointer)

                    codePointer <- codePointer + 4

                    let code =
                        BytecodeElement.deserializeInt (bc.code, codePointer)

                    codePointer <- codePointer + 4

                    let closure =
                        Value.Closure { env = env; var = var; code = code }

                    stack.Push(closure)
                | OpCodeNum.Apply ->
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

                    codePointer <- newCodePointer
                    env <- newEnv
                | OpCodeNum.Return ->
                    let contCodePointer, contEnv = returnStack.Pop()
                    codePointer <- contCodePointer
                    env <- contEnv
                | other -> raise (InterpException(Internal $"Unknown opcode: {other}"))

        member this.Execute() =
            codePointer <- bc.entry
            this.RunLoop()

        member this.CurrentValue: Option<Value> =
            if stack.Count = 0 then
                None
            else
                Some(stack.Peek())

module Ex =
    open FlatVM

    let eval expr =
        let startTs = DateTime.Now

        let bc = SB.genBytecode expr |> Bytecode.build
        let bcDoneTs = DateTime.Now

        let vm = VM(bc)

        try
            vm.Execute()
            let finishTs = DateTime.Now
            let value = vm.CurrentValue

            let bcDur = bcDoneTs - startTs
            let exeDur = finishTs - bcDoneTs
            printfn $"[{bcDur.TotalMilliseconds}/{exeDur.TotalMilliseconds}>> {value}"
        with
        | InterpException err -> printfn $"!! {err}"

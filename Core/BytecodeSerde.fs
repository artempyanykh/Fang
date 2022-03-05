module Fang.BytecodeSerde

open System
module BC = Bytecode

type Bytecode = { code: array<byte>; entry: int }

module OpCodeNum =
    [<Literal>]
    let Halt = 255uy

    [<Literal>]
    let Nop = 0uy

    [<Literal>]
    let Blackhole = 254uy

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
    let IntNeg = 15uy

    [<Literal>]
    let IntLess = 20uy

    [<Literal>]
    let IntEqual = 21uy

    [<Literal>]
    let IntGreater = 22uy

    [<Literal>]
    let EnvLoad = 30uy

    [<Literal>]
    let EnvSave = 31uy

    [<Literal>]
    let EnvUpdate = 33uy

    [<Literal>]
    let EnvRestore = 32uy

    [<Literal>]
    let Jump = 40uy

    [<Literal>]
    let JumpTrue = 41uy

    [<Literal>]
    let MakeClosure = 50uy

    [<Literal>]
    let Apply = 51uy

    [<Literal>]
    let Return = 52uy

[<RequireQualifiedAccess>]
type JumpTarget =
    | Resolved of codePointer: int
    | Unresolved of labelNum: int

type BytecodeElement =
    | OpCode of byte
    | ImmInt of int
    | JumpTarget of JumpTarget

module BytecodeElement =
    open BC

    let fromInstr (i: Instr) : List<BytecodeElement> =
        match i with
        | Instr.Halt -> [ OpCode OpCodeNum.Halt ]
        | Instr.Nop -> [ OpCode OpCodeNum.Nop ]
        | Instr.Blackhole -> [ OpCode OpCodeNum.Blackhole ]
        | Instr.IntConst v -> [ OpCode OpCodeNum.IntConst; ImmInt v ]
        | Instr.IntBinaryOp op ->
            let code =
                match op with
                | IntBinaryOp.Add -> OpCodeNum.IntAdd
                | IntBinaryOp.Sub -> OpCodeNum.IntSub
                | IntBinaryOp.Mul -> OpCodeNum.IntMul
                | IntBinaryOp.Div -> OpCodeNum.IntDiv
                | IntBinaryOp.Less -> OpCodeNum.IntLess
                | IntBinaryOp.Equal -> OpCodeNum.IntEqual
                | IntBinaryOp.Greater -> OpCodeNum.IntGreater

            [ OpCode code ]
        | Instr.IntUnaryOp op ->
            let code =
                match op with
                | IntUnaryOp.Neg -> OpCodeNum.IntNeg

            [ OpCode code ]
        | Instr.EnvLoad i -> [ OpCode OpCodeNum.EnvLoad; ImmInt i ]
        | Instr.EnvSave i -> [ OpCode OpCodeNum.EnvSave; ImmInt i ]
        | Instr.EnvRestore i ->
            [ OpCode OpCodeNum.EnvRestore
              ImmInt i ]
        | Instr.EnvUpdate i -> [ OpCode OpCodeNum.EnvUpdate; ImmInt i ]
        | Instr.Jump target ->
            [ OpCode OpCodeNum.Jump
              JumpTarget(JumpTarget.Unresolved target) ]
        | Instr.JumpTrue target ->
            [ OpCode OpCodeNum.JumpTrue
              JumpTarget(JumpTarget.Unresolved target) ]
        | Instr.MakeClosure (i, code) ->
            [ OpCode OpCodeNum.MakeClosure
              ImmInt i
              JumpTarget(JumpTarget.Unresolved code) ]
        | Instr.Apply -> [ OpCode OpCodeNum.Apply ]
        | Instr.Return -> [ OpCode OpCodeNum.Return ]


    let byteWidth =
        function
        | OpCode _ -> 1
        | ImmInt _ -> 4
        | JumpTarget _ -> 4

    let serializeInt (num: int) : array<byte> =
        let bytes = BitConverter.GetBytes(num)

        if not BitConverter.IsLittleEndian then
            Array.Reverse(bytes)

        bytes

    let deserializeInt (source: array<byte>, start: int) : int =

        if BitConverter.IsLittleEndian then
            BitConverter.ToInt32(source, start)
        else
            let bytes = source.[start..start + 3]
            Array.Reverse(bytes)
            BitConverter.ToInt32(bytes)


    let bytes (el: BytecodeElement) : seq<byte> =
        match el with
        | OpCode c -> [ byte c ]
        | ImmInt i -> serializeInt i
        | JumpTarget (JumpTarget.Resolved codePointer) -> serializeInt codePointer
        | JumpTarget (JumpTarget.Unresolved labelNum) ->
            failwith $"Unresolved jump target cannot be converted: {labelNum}"

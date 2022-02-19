module Fang.Bytecode

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

[<RequireQualifiedAccess>]
type Instr =
    | Halt
    | IntConst of int
    | IntBinaryOp of IntBinaryOp
    | IntUnaryOp of IntUnaryOp

type Bytecode = array<Instr>

type BytecodeBuilder() =

    let bytecodeBuffer = ResizeArray()

    member private this.EmitInstr(instr: Instr) = bytecodeBuffer.Add(instr)

    member this.Generate(expr: Expr) =
        match expr with
        | Lit (BType.Int i) -> this.EmitInstr(Instr.IntConst i)
        | Lit BType.Unit -> this.EmitInstr(Instr.IntConst 0)
        | Builtin (BuiltinFn.Arithmetic (fn, opA, opB)) ->
            this.Generate(opA) // stack: valueA
            this.Generate(opB) // stack: valueA valueB

            let opType = IntBinaryOp.fromArithmeticFn fn
            this.EmitInstr(Instr.IntBinaryOp opType)
        | Builtin (BuiltinFn.Comparison (fn, lhs, rhs)) ->
            this.Generate(lhs) // stack: valueL
            this.Generate(rhs) // stack: valueL valueR

            let opType = IntBinaryOp.fromComparisonFn fn
            this.EmitInstr(Instr.IntBinaryOp opType)
        | Builtin (BuiltinFn.UnaryArithmetic (fn, expr)) ->
            this.Generate(expr)

            match fn with
            | Neg -> this.EmitInstr(Instr.IntUnaryOp IntUnaryOp.Neg)


    member this.Build() = Array.ofSeq bytecodeBuffer

let generateBytecode (expr: Expr) : Bytecode =
    let bb = BytecodeBuilder()
    bb.Generate(expr)
    bb.Build()

module VM =
    open System.Collections.Generic

    [<RequireQualifiedAccess>]
    type Value =
        | Int of int
        | Blackhole

    module Value =
        let asInt v =
            match v with
            | Value.Int i -> i
            | other -> failwith $"Expected int value: {other}"

    type VM(bc: Bytecode) =
        let stack: Stack<Value> = Stack()
        let mutable codePointer: int = 0

        member this.Execute() =
            let mutable shouldHalt = false

            while (codePointer < bc.Length && not shouldHalt) do
                let instr = bc.[codePointer]
                codePointer <- codePointer + 1

                match instr with
                | Instr.Halt -> shouldHalt <- true
                | Instr.IntConst i -> stack.Push(Value.Int i)
                | Instr.IntBinaryOp intBinaryOp ->
                    let arg2 = stack.Pop() |> Value.asInt
                    let arg1 = stack.Pop() |> Value.asInt

                    let result =
                        match intBinaryOp with
                        | IntBinaryOp.Add -> arg1 + arg2
                        | IntBinaryOp.Sub -> arg1 - arg2
                        | IntBinaryOp.Mul -> arg1 * arg2
                        | IntBinaryOp.Div -> arg1 / arg2
                        | IntBinaryOp.Less -> if arg1 < arg2 then 1 else 0
                        | IntBinaryOp.Equal -> if arg1 = arg2 then 1 else 0
                        | IntBinaryOp.Greater -> if arg1 > arg2 then 1 else 0

                    stack.Push(Value.Int result)
                | Instr.IntUnaryOp intUnaryOp ->
                    let arg = stack.Pop() |> Value.asInt

                    let result =
                        match intUnaryOp with
                        | IntUnaryOp.Neg -> -arg

                    stack.Push(Value.Int result)

        member this.CurrentValue() : Option<Value> =
            if stack.Count = 0 then
                None
            else
                Some(stack.Peek())

module Ex =
    let eval (expr: Expr) : VM.Value =
        let bc = generateBytecode expr
        let vm = VM.VM(bc)
        vm.Execute()
        vm.CurrentValue() |> Option.get

    let evalPrint expr =
        let start = System.DateTime.Now

        try
            let expr = eval expr
            let finish = System.DateTime.Now
            let duration = finish - start
            $"[{duration.TotalMilliseconds}ms]>> {expr}"
        with
        | err -> $"!! {err}"

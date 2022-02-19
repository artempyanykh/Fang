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

type ConstPool() =
    let name2num: Dictionary<string, ConstNum> = Dictionary()
    let num2name: ResizeArray<string> = ResizeArray()

    member this.Add(name: string) : ConstNum =
        if name2num.ContainsKey(name) then
            name2num.[name]
        else
            let nextNum = num2name.Count
            num2name.Add(name)
            name2num.Add(name, nextNum)
            nextNum

    member this.FindName(num: ConstNum) : string = num2name.[num]

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

type Bytecode = array<Instr>

type BytecodeBuilder() =
    let constPool = ConstPool()

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
        | Var name ->
            let nameNum = constPool.Add(name)
            this.EmitInstr(Instr.EnvLoad nameNum)
        | Bind (recursive, var, body, expr) ->
            let varNum = constPool.Add(var)

            if not recursive then
                this.Generate(body)
                this.EmitInstr(Instr.EnvSave varNum)
                this.Generate(expr)
                this.EmitInstr(Instr.EnvRestore varNum)
            else
                this.EmitInstr(Instr.Blackhole)
                this.EmitInstr(Instr.EnvSave varNum)
                this.Generate(body)
                this.EmitInstr(Instr.EnvUpdate varNum)

                this.Generate(expr)
                this.EmitInstr(Instr.EnvRestore varNum)


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

    type Env = Map<ConstNum, Value>

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

    type VM(bc: Bytecode) =
        let stack: Stack<Value> = Stack()
        let mutable codePointer: int = 0

        let mutable env: Env = Env.empty
        let envStack: Stack<Env> = Stack()

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
                | Instr.EnvLoad n ->
                    let value = Env.find n env
                    stack.Push(value)
                | Instr.EnvSave n ->
                    let value = stack.Pop()
                    envStack.Push(env)

                    env <- Map.add n value env
                | Instr.EnvUpdate n ->
                    let value = stack.Pop()
                    env <- Map.add n value env
                | Instr.EnvRestore _ -> env <- envStack.Pop()

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

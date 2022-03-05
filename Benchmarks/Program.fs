module Fang.Benchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type InterpreterType =
    | Tree = 0
    | Bytecode = 1

type ExampleName =
    | Arithmetic = 0
    | ArithmeticBinds = 1
    | PlusConst = 2
    | PlusN = 3
    | FibDirect10 = 4
    | FibDirect20 = 5
    | FibFixpoint10 = 6
    | FibFixpoint20 = 7


type EvalBench() =
    let examples =
        [ Example.AST.arithmetic
          Example.AST.arithmeticBinds
          Example.AST.plus42
          Example.AST.plusN
          Example.AST.fibDirect 10
          Example.AST.fibDirect 20
          Example.AST.fibFixpoint 10
          Example.AST.fibFixpoint 20 ]

    [<ParamsAllValues>]
    member val public interpreterType: InterpreterType = InterpreterType.Tree with get, set

    [<ParamsAllValues>]
    member val public exampleNum: ExampleName = ExampleName.Arithmetic with get, set

    member val public benchmarkThunk: unit -> unit = (fun _ -> ()) with get, set

    [<GlobalSetup>]
    member self.setup() =
        let ast = examples.[int self.exampleNum]

        match self.interpreterType with
        | InterpreterType.Tree -> self.benchmarkThunk <- fun _ -> TreeInterpreter.Ex.evalExpr ast |> ignore
        | InterpreterType.Bytecode ->
            let bc = Bytecode.genBytecode ast
            self.benchmarkThunk <- fun _ -> Bytecode.Ex.evalBytecode bc |> ignore
        | other -> failwith $"Unknown interpreter type: {other}"

    [<Benchmark>]
    member self.evalTime() = self.benchmarkThunk ()


BenchmarkRunner.Run<EvalBench>() |> ignore

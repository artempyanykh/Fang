module Fang.Benchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type InterpreterType =
    | Tree = 0
    | SymbolicBC = 1
    | FlatBC = 2

type EvalBench() =
    let interpreters =
        [ TreeInterpreter.Ex.evalExpr >> ignore
          SymbolicBytecode.Ex.evalExpr >> ignore
          FlatBytecode.Ex.evalExpr >> ignore ]

    [<ParamsAllValues>]
    member val public interpreterType: InterpreterType = InterpreterType.Tree with get, set

    [<Benchmark>]
    member self.arithmetic() =
        interpreters.[int self.interpreterType] Example.AST.arithmetic

    [<Benchmark>]
    member self.arithmeticBinds() =
        interpreters.[int self.interpreterType] Example.AST.arithmeticBinds

    [<Benchmark>]
    member self.plus42() =
        interpreters.[int self.interpreterType] Example.AST.plus42

    [<Benchmark>]
    member self.plusN() =
        interpreters.[int self.interpreterType] Example.AST.plusN

    [<Benchmark>]
    member self.fibDirect10() =
        interpreters.[int self.interpreterType] (Example.AST.fibDirect 10)

    [<Benchmark>]
    member self.fibDirect20() =
        interpreters.[int self.interpreterType] (Example.AST.fibDirect 20)

    [<Benchmark>]
    member self.fibFixpoint10() =
        interpreters.[int self.interpreterType] (Example.AST.fibFixpoint 10)

    member self.fibFixpoint20() =
        interpreters.[int self.interpreterType] (Example.AST.fibFixpoint 20)

BenchmarkRunner.Run<EvalBench>() |> ignore

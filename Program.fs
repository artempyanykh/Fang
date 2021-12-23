module Fang.Program

// Topic 1: extending lambda calculus with builtins and literals
// Topic 2: recursion, fixpoint operator, let bindings
// Topic 3: AST based eval
// Topic 4: IR or byte-code

[<EntryPoint>]
let main args =
    //    printfn "Minimal Eager ->"
//    MinimalInterpreter.EvalEager.runExamples ()
//
    printfn "With closure and let bindings ->"
    Interpreter.Ex.runExamples ()

    printfn "Symbolic bytecode interpreter ->"
    SymbolicBytecode.Ex.eval Example.AST.arithmetic
    SymbolicBytecode.Ex.eval Example.AST.arithmeticBinds
    SymbolicBytecode.Ex.eval Example.AST.plus42
    SymbolicBytecode.Ex.eval Example.AST.plusN
    SymbolicBytecode.Ex.eval (Example.AST.fib 4)
    SymbolicBytecode.Ex.eval (Example.AST.fib 20)
    SymbolicBytecode.Ex.eval (Example.AST.fib 26)
    SymbolicBytecode.Ex.eval (Example.AST.fib 30)
    
    printfn "Flat bytecode interpreter ->"
    FlatBytecode.Ex.eval Example.AST.arithmetic
    FlatBytecode.Ex.eval Example.AST.arithmeticBinds
    FlatBytecode.Ex.eval Example.AST.plus42
    FlatBytecode.Ex.eval Example.AST.plusN
    FlatBytecode.Ex.eval (Example.AST.fib 4)
    FlatBytecode.Ex.eval (Example.AST.fib 20)
    FlatBytecode.Ex.eval (Example.AST.fib 26)
    FlatBytecode.Ex.eval (Example.AST.fib 30)

    0

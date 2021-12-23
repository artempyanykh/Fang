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
//    printfn "With closure and let bindings ->"
//    Interpreter.Ex.runExamples ()

    printfn "Bytecode interpreter ->"
    Bytecode.Ex.eval Bytecode.Ex.ex1_AST
    Bytecode.Ex.eval Bytecode.Ex.ex2_AST
    Bytecode.Ex.eval Bytecode.Ex.ex3_AST
    Bytecode.Ex.eval Bytecode.Ex.ex4_AST
    Bytecode.Ex.eval (Bytecode.Ex.ex5_AST 4)
    Bytecode.Ex.eval (Bytecode.Ex.ex5_AST 20)
    Bytecode.Ex.eval (Bytecode.Ex.ex5_AST 26)
    Bytecode.Ex.eval (Bytecode.Ex.ex5_AST 30)

    0

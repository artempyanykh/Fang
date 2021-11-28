module Fang.Program

// Topic 1: extending lambda calculus with builtins and literals
// Topic 2: recursion, fixpoint operator, let bindings
// Topic 3: AST based eval
// Topic 4: IR or byte-code

[<EntryPoint>]
let main args =
//    printfn "Lazy ->"
//    Minimal.EvalLazy.runExamples ()
//    Minimal.EvalLazy.runExamples ()
//    
//    printfn "Eager ->"
//    Minimal.EvalEager.runExamples ()
//    Minimal.EvalEager.runExamples ()
    Interp.Ex.runExamples ()
    
    0

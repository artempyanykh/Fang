module Fang.Program

// Topic 1: extending lambda calculus with builtins and literals
// Topic 2: recursion, fixpoint operator, let bindings
// Topic 3: AST based eval
// Topic 4: IR or byte-code

[<EntryPoint>]
let main args =
    printfn "Minimal Eager ->"
    Minimal.EvalEager.runExamples ()

    printfn "With closure and let bindings ->"
    Interp.Ex.runExamples ()

    0

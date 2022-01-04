module Fang.Program

// Topic 1: extending lambda calculus with builtins and literals
// Topic 2: recursion, fixpoint operator, let bindings
// Topic 3: AST based eval
// Topic 4: IR or byte-code

[<EntryPoint>]
let main args =
    let examples =
        [ "arithmetic", Example.AST.arithmetic
          "arithmeticBinds", Example.AST.arithmeticBinds
          "plus42", Example.AST.plus42
          "plusN", Example.AST.plusN
          "fibDirect 10", (Example.AST.fibDirect 10)
          "fibDirect 20", (Example.AST.fibDirect 20)
          "fibDirect 26", (Example.AST.fibDirect 26)
          "fibDirect 30", (Example.AST.fibDirect 30)
          "fibFixpoint 10", (Example.AST.fibFixpoint 10)
          "fibFixpoint 20", (Example.AST.fibFixpoint 20)
          "fibFixpoint 26", (Example.AST.fibFixpoint 26)
          "fibFixpoint 30", (Example.AST.fibFixpoint 30)
          "badRecursiveBind", Example.AST.badRecursiveBind ]

    let interpreters =
        [ "Tree-based interpreter", TreeInterpreter.Ex.evalPrint
          "Chunked Bytecode", ChunkedBytecode.Ex.evalPrint
          "Flat Bytecode", FlatBytecode.Ex.evalPrint ]

    for exampleName, example in examples do
        printfn $"{exampleName}"

        for interpreterName, interpreter in interpreters do
            printfn $"{interpreterName}"
            interpreter example

    0

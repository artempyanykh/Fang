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
          "fibDirect 4", (Example.AST.fibDirect 4)
          "fibDirect 20", (Example.AST.fibDirect 20)
          "fibDirect 26", (Example.AST.fibDirect 26)
          "fibDirect 30", (Example.AST.fibDirect 30)
          "fibFixpoint 10", (Example.AST.fibFixpoint 10)
          "fibFixpoint 20", (Example.AST.fibFixpoint 20) ]

    let interpreters =
        [ "Tree-based interpreter", Interpreter.Ex.evalPrint
          "Symbolic Bytecode", SymbolicBytecode.Ex.evalPrint
          "Flat Bytecode", FlatBytecode.Ex.evalPrint ]

    for exampleName, example in examples do
        printfn $"{exampleName}"

        for interpreterName, interpreter in interpreters do
            printfn $"{interpreterName}"
            interpreter (example)

    0

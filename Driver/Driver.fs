module Fang.Driver

let runExamples () =
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
          "Symbolic Bytecode", SymbolicBytecode.Ex.evalPrint
          "New Bytecode", Bytecode.Ex.evalPrint
          "Flat Bytecode", FlatBytecode.Ex.evalPrint ]

    for exampleName, example in examples do
        printfn $"{exampleName}"

        for interpreterName, interpreter in interpreters do
            printfn $"{interpreterName}"
            printfn $"{interpreter example}"

    0

module CLI =
    open CommandLine

    [<Verb("compile", HelpText = "Compile Fang source code.")>]
    type CompileOpts =
        { [<Value(0, Required = true, MetaName = "file", HelpText = "Source code file to compile.")>]
          file: string }

    [<Verb("run", HelpText = "Run compiled Fang program.")>]
    type RunOpts =
        { [<Value(0, Required = true, MetaName = "file", HelpText = "Compiled program to run.")>]
          file: string }

    [<Verb("demo", HelpText = "Run demo programs with timings.")>]
    type DemoOpts() =
        class
        end


    type Command =
        | Compile of CompileOpts
        | Run of RunOpts
        | Demo of DemoOpts

    let inline (|Success|Help|Version|Fail|) (result: ParserResult<'a>) =
        match result with
        | :? Parsed<'a> as parsed -> Success(parsed.Value)
        | :? NotParsed<'a> as notParsed when notParsed.Errors.IsHelp() -> Help
        | :? NotParsed<'a> as notParsed when notParsed.Errors.IsVersion() -> Version
        | :? NotParsed<'a> as notParsed -> Fail(notParsed.Errors)
        | _ -> failwith "invalid parser result"

    let parse args =
        let result =
            Parser.Default.ParseArguments<CompileOpts, RunOpts, DemoOpts> args

        match result with
        | Success opts ->
            match opts with
            | :? CompileOpts as opts -> Some(Compile opts)
            | :? RunOpts as opts -> Some(Run opts)
            | :? DemoOpts as opts -> Some(Demo opts)
            | other -> failwith $"Unexpected command: {other}"
        | _ -> None

let run args =
    let result = CLI.parse args

    match result with
    | Some cmd ->
        match cmd with
        | CLI.Command.Demo _ -> runExamples ()
        | CLI.Command.Compile _
        | CLI.Command.Run _ ->
            printfn $"Command not implemented: {cmd}"
            0
    | None -> 1

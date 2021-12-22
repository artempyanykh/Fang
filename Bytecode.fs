module Fang.Bytecode

open Fang.Lang

[<RequireQualifiedAccess>]
type IntOp =
    | Add
    | Sub
    | Mul
    | Div
    
[<RequireQualifiedAccess>]
type IntCmp =
    | Less
    | Equal
    | Greater

type Instr =
    | Halt
    | Bottom
    | IntConst of int
    | IntOp of IntOp
    | IntCmp of IntCmp
    | Access of nameId:int
    | Save of nameId:int
    | Drop of nameId:int
    | SaveRec of nameId:int
    | Jump of labelId:int
    | Branch of trueId:int * falseId:int
    | MkClosure of nameId:int * labelId:int
    | Apply

type ConstPool() =
    let mutable mapping: Map<string, int> = Map.empty

    member self.Add(value: string) : int =
        match Map.tryFind value mapping with
        | Some num -> num
        | _ ->
            let nextNum = mapping.Count
            mapping <- Map.add value nextNum mapping
            nextNum

    member self.Find(value: string) = Map.find value mapping

    member self.ReverseMapping() : array<string> =
        let rev = Array.zeroCreate mapping.Count

        for entry in mapping do
            rev.[entry.Value] <- entry.Key

        rev

type Label = string

type LabelManager() =
    let mutable mapping: Map<Label, int> =
        Map.ofList [ LabelManager.EntryLabel, 0 ]

    member self.GenLabel() : Label * int =
        let nextId = mapping.Count
        let label = $"gen_lbl_{nextId}"
        mapping <- Map.add label nextId mapping
        label, nextId

    member self.GenNamedLabel(name: string) : Label * int =
        let nextId = mapping.Count
        let label = $"{name}_lbl_{nextId}"
        mapping <- Map.add label nextId mapping
        label, nextId

    member self.GenScopedLabel(scope: Label, name: string) : Label * int =
        let nextId = mapping.Count
        let label = $"{scope}_{name}_lbl_{nextId}"
        mapping <- Map.add label nextId mapping
        label, nextId

    member self.FindLabelId(label: Label) : int = Map.find label mapping

    member self.ReverseMapping() : array<string> =
        let rev = Array.zeroCreate mapping.Count

        for entry in mapping do
            rev.[entry.Value] <- entry.Key

        rev
        
    static member EntryLabel: Label = "entry"

type Bytecode() =
    let mutable chunks: Map<Label, List<Instr>> = Map.empty
    member val ConstPool: ConstPool = ConstPool()
    member val LabelManager: LabelManager = LabelManager()

    member self.EmitInstr(label: Label, instr: Instr) : unit =
        if not (chunks.ContainsKey label) then
            chunks <- Map.add label List.Empty chunks

        let cur = Map.find label chunks
        chunks <- Map.add label (List.append cur [ instr ]) chunks

    member self.GetInstructions(label: Label) : List<Instr> = Map.find label chunks


let rec genBytecodeImpl (bc: Bytecode) (initLabel: Label) (expr: Expr) : unit =
    match expr with
    | Lit (BType.Int v) -> bc.EmitInstr(initLabel, IntConst v)
    | Lit BType.Unit -> bc.EmitInstr(initLabel, IntConst 0)
    | Var (VarName var) ->
        let constNum = bc.ConstPool.Add var
        bc.EmitInstr(initLabel, Access constNum)
    | Lam (VarName var, body) ->
        let lamBodyLabel, lamBodyLabelNum = bc.LabelManager.GenScopedLabel(initLabel, "lam")
        genBytecodeImpl bc lamBodyLabel body
        
        let varNum = bc.ConstPool.Add var
        bc.EmitInstr(initLabel, MkClosure(varNum, lamBodyLabelNum))
    | App (expr, arg) ->
        genBytecodeImpl bc initLabel arg
        genBytecodeImpl bc initLabel expr
        bc.EmitInstr(initLabel, Apply)
    | Cond (p, t, f) ->
        let tbl, tblNum = bc.LabelManager.GenScopedLabel(initLabel, "tb")
        genBytecodeImpl bc tbl t
        
        let fbl, fblNum = bc.LabelManager.GenScopedLabel(initLabel, "fbl")
        genBytecodeImpl bc fbl f
        
        genBytecodeImpl bc initLabel p
        bc.EmitInstr(initLabel, Branch (tblNum, fblNum))
    | Builtin (Arithmetic (arithmeticFn, opA, opB)) ->
        genBytecodeImpl bc initLabel opA
        genBytecodeImpl bc initLabel opB

        match arithmeticFn with
        | Add -> bc.EmitInstr(initLabel, IntOp IntOp.Add)
        | Sub -> bc.EmitInstr(initLabel, IntOp IntOp.Sub)
        | Mul -> bc.EmitInstr(initLabel, IntOp IntOp.Mul)
        | Div -> bc.EmitInstr(initLabel, IntOp IntOp.Div)
    | Builtin (Comparison (comparisonFn, lhs, rhs)) ->
        genBytecodeImpl bc initLabel lhs
        genBytecodeImpl bc initLabel rhs

        match comparisonFn with
        | Less -> bc.EmitInstr(initLabel, IntCmp IntCmp.Less)
        | Equal -> bc.EmitInstr(initLabel, IntCmp IntCmp.Equal)
        | Greater -> bc.EmitInstr(initLabel, IntCmp IntCmp.Greater)
    | Bind (recursive, VarName var, body, expr) ->
        let varConst = bc.ConstPool.Add var

        if recursive then
            let bodyLabel, bodyLabelNum = bc.LabelManager.GenScopedLabel(initLabel, $"bind_{var}")
            let contLabel, contLabelNum = bc.LabelManager.GenScopedLabel(initLabel, $"bind_{var}_cont")
            
            genBytecodeImpl bc bodyLabel body
            bc.EmitInstr(bodyLabel, Jump contLabelNum)
            
            bc.EmitInstr(initLabel, Bottom)
            bc.EmitInstr(initLabel, Save varConst)
            bc.EmitInstr(initLabel, Jump bodyLabelNum)

            bc.EmitInstr(contLabel, Drop varConst)
            bc.EmitInstr(contLabel, SaveRec varConst)
            genBytecodeImpl bc contLabel expr
            bc.EmitInstr(contLabel, Drop varConst)
        else
            genBytecodeImpl bc initLabel body
            bc.EmitInstr(initLabel, Save varConst)
            genBytecodeImpl bc initLabel expr
            bc.EmitInstr(initLabel, Drop varConst)

let genBytecode (expr: Expr) : Bytecode =
    let bc = Bytecode()
    genBytecodeImpl bc LabelManager.EntryLabel expr
    bc.EmitInstr(LabelManager.EntryLabel, Halt)
    bc

module VM =
        
    [<RequireQualifiedAccess>]
    type Value =
        | Bottom
        | Int of int
        | Closure of Closure
        | ClosureRec of Closure * recNameId:int
    and Closure = {env:Env ; varId:int ; labelId:int}
    and Env =
        { cur: Map<int, Value>
          prev: Option<Env> }
        
    let envEmpty = { cur = Map.empty; prev = None }
    
    let envTryFind (constId: int) (env: Env) : Option<Value> = Map.tryFind constId env.cur
    
    let envWithBind (constId: int) (value: Value) (env: Env) : Env =
        let cur = Map.add constId value env.cur
        { cur = cur; prev = Some env }

    type InterpError =
        | WrongType of Value * expected: string
        | UnboundVar of name: string
        | AccessUneval of name: string
        | Internal of msg: string

    exception InterpException of InterpError

    let valueAsIntExn =
        function
        | Value.Int i -> i
        | other -> raise (InterpException(WrongType(other, "int")))
        
    let valueAsClosureExn =
        function
        | Value.Closure c -> c
        | other -> raise (InterpException(WrongType(other, "closure")))

    type VM(bc: Bytecode) =
        let mutable stack: System.Collections.Generic.Stack<Value> = System.Collections.Generic.Stack()
        let constLookup = bc.ConstPool.ReverseMapping()
        let labelLookup = bc.LabelManager.ReverseMapping()

        let mutable env: Env = envEmpty

        member private self.ExecuteImpl(instructions: List<Instr>) =
            let mutable shouldHalt = false

            for instr in instructions do
                if not shouldHalt then
                    match instr with
                    | Halt -> shouldHalt <- true
                    | Bottom -> stack.Push(Value.Bottom)
                    | IntConst i -> stack.Push(Value.Int i)
                    | IntOp op ->
                        let arg2 = stack.Pop() |> valueAsIntExn
                        let arg1 = stack.Pop() |> valueAsIntExn

                        let result =
                            match op with
                            | IntOp.Add -> arg1 + arg2
                            | IntOp.Sub -> arg1 - arg2
                            | IntOp.Mul -> arg1 * arg2
                            | IntOp.Div -> arg1 / arg2

                        stack.Push(Value.Int result)
                    | IntCmp op ->
                        let arg2 = stack.Pop() |> valueAsIntExn
                        let arg1 = stack.Pop() |> valueAsIntExn

                        let result =
                            match op with
                            | IntCmp.Less -> if arg1 < arg2 then 1 else 0
                            | IntCmp.Equal -> if arg1 = arg2 then 1 else 0
                            | IntCmp.Greater -> if arg1 > arg2 then 1 else 0

                        stack.Push(Value.Int result)
                    | Access nameId ->
                        match envTryFind nameId env with
                        | Some v ->
                            match v with
                            | Value.Bottom -> raise (InterpException(AccessUneval constLookup[nameId]))
                            | _ -> stack.Push(v)
                        | None -> raise (InterpException(UnboundVar constLookup[nameId]))
                    | Save nameId ->
                        let value = stack.Pop()
                        env <- envWithBind nameId value env
                    | Drop nameId ->
                        match env.prev with
                        | Some prev -> env <- prev
                        | _ -> raise (InterpException(Internal $"Env violation: drop {constLookup[nameId]} with empty previous env"))
                    | SaveRec nameId ->
                        // TODO: need to handle ClosureRec too?
                        let closureValue = stack.Pop() |> valueAsClosureExn
                        let closureRec = Value.ClosureRec(closureValue, nameId)
                        env <- envWithBind nameId closureRec env
                    | Jump labelId ->
                        let nextInstructions = bc.GetInstructions(labelLookup[labelId])
                        // TODO: this is ridiculous
                        shouldHalt <- true
                        self.ExecuteImpl(nextInstructions)
                    | Branch (t, f) ->
                        let value = stack.Pop() |> valueAsIntExn
                        let target = if value <> 0 then t else f
                        let nextInstructions = bc.GetInstructions(labelLookup[target])
                        // TODO: this is ridiculous
                        shouldHalt <- true
                        self.ExecuteImpl(nextInstructions)
                    | MkClosure(nameId, labelId) ->
                        let closure = Value.Closure {env = env; varId = nameId; labelId = labelId}
                        stack.Push(closure)
                    | Apply ->
                        let value = stack.Pop()
                        match value with
                        | Value.Closure closure ->
                            let arg = stack.Pop()
                            let savedEnv = env
                            env <- envWithBind closure.varId arg closure.env
                            let nextInstructions = bc.GetInstructions(labelLookup[closure.labelId])
                            self.ExecuteImpl(nextInstructions)
                            env <- savedEnv
                        | Value.ClosureRec(closure, recNameId) ->
                            let arg = stack.Pop()
                            let savedEnv = env
                            env <- closure.env |> envWithBind closure.varId arg |> envWithBind recNameId value
                            let nextInstructions = bc.GetInstructions(labelLookup[closure.labelId])
                            self.ExecuteImpl(nextInstructions)
                            env <- savedEnv
                        | other -> raise (InterpException(WrongType(other, "closure"))) 

        member self.Execute() =
            let main =
                bc.GetInstructions(LabelManager.EntryLabel)

            self.ExecuteImpl(main)

        member self.CurrentValue: Option<Value> =
            if stack.Count = 0 then
                None
            else
                Some(stack.Peek())

open VM

module Ex =
    let eval expr =
        let startTs = System.DateTime.Now
        
        let bc = genBytecode expr
        let bcDoneTs = System.DateTime.Now
        
        let vm = VM.VM(bc)
        try
            vm.Execute()
            let finishTs = System.DateTime.Now
            let value = vm.CurrentValue
            
            let bcDur = bcDoneTs - startTs
            let exeDur = finishTs - bcDoneTs
            printfn $"[{bcDur.TotalMilliseconds}/{exeDur.TotalMilliseconds}>> {value}"
        with
        | InterpException err -> printfn $"!! {err}"
        
    let ex1_AST =
        // 2 + 3 * 4
        Builtin(
            Arithmetic( //
                Add,
                Lit(BType.Int 2),
                Builtin(
                    Arithmetic( //
                        Mul,
                        Lit(BType.Int 3),
                        Lit(BType.Int 4)
                    )
                )
            )
        )

    let ex2_AST =
        // let x = 2 + 3 * 4 in let y = 42 in x - y
        Bind(
            recursive = false,
            var = VarName "x",
            body = ex1_AST,
            expr =
                Bind(
                    recursive = false,
                    var = VarName "y",
                    body = Lit(BType.Int 42),
                    expr =
                        Builtin(Arithmetic(Sub, Var (VarName "x"), Var (VarName "y")))
                    )
            )
        
    let ex3_AST =
        Bind(
            recursive = false,
            var = VarName "plus42",
            body = Lam(VarName "x", Builtin(Arithmetic(Add, Var (VarName "x"), Lit (BType.Int 42)))),
            expr =
                App(Var (VarName "plus42"), Lit (BType.Int 3))
            )
        
    let ex4_AST =
        Bind(
            recursive = false,
            var = VarName "plusN",
            body = Lam(VarName "N",
                       Lam(VarName "x",
                           Builtin(Arithmetic(Add, Var (VarName "x"), Var (VarName "N"))))),
            expr =
                App(
                    App(Var (VarName "plusN"), Lit (BType.Int 3)),
                    Lit (BType.Int 5)))
            

    let ex5_AST (num: int) =
        let fib = VarName "fib"
        let n = VarName "n"
        let one = Lit(BType.Int 1)
        let two = Lit(BType.Int 2)
        let leqOne = Builtin(Comparison(Less, Var n, two))

        let fn1 =
            App(Var fib, Builtin(Arithmetic(Sub, Var n, one)))

        let fn2 =
            App(Var fib, Builtin(Arithmetic(Sub, Var n, two)))

        Bind(
            recursive = true,
            var = fib,
            body = Lam(n, Cond(leqOne, one, Builtin(Arithmetic(Add, fn1, fn2)))),
            expr = App(Var fib, Lit (BType.Int num))
        )

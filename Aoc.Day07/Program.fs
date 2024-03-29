open System
open System.IO

open FSharpPlus
open FSharpPlus.Data

type OpCode =
    | Add
    | Multiply
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | Less
    | Equals
    | Halt

type ParameterMode =
    | Position
    | Immediate

type Instruction = {
    Code : OpCode
    ParameterModes : ParameterMode list
}

type IntCodeMemory = IntCodeMemory of int array

type IntCodeState = {
    InstructionPointer : int
    Input : int list
    Output : int list
    IsPaused : bool
    IsHalted : bool
}

let opCode code =
    match code with
    | 1 -> Some Add
    | 2 -> Some Multiply
    | 3 -> Some Input
    | 4 -> Some Output
    | 5 -> Some JumpIfTrue
    | 6 -> Some JumpIfFalse
    | 7 -> Some Less
    | 8 -> Some Equals
    | 99 -> Some Halt
    | _ -> None

let flippedDigits = List.unfold (fun num -> if num = 0 then None else Some (num % 10, num / 10))

let parameterMode mode =
    match mode with
    | 0 -> Some Position
    | 1 -> Some Immediate
    | _ -> None

let numParameters =
    function
    | Add -> 3
    | Multiply -> 3
    | Input -> 1
    | Output -> 1
    | JumpIfTrue -> 2
    | JumpIfFalse -> 2
    | Less -> 3
    | Equals -> 3
    | Halt -> 0

let toInstruction instruction = monad {
    let! code = instruction % 100 |> opCode
    let! modes =
        instruction / 100
        |> flippedDigits
        |> List.map parameterMode
        |> sequence

    let expected = code |> numParameters
    let actual = modes |> List.length

    return { Code = code; ParameterModes = modes @ List.replicate (expected - actual) Position }
}

let setCodeAt target newValue code =
    code |> Array.mapi (fun index item -> if index = target then newValue else item)

let getParameter position mode (memory : int array) =
    match mode with
    | Position -> memory.[memory.[position]]
    | Immediate -> memory.[position]

let execStep (IntCodeMemory memory) = monad {
    let! state = State.get

    let performBinary op (mode1, mode2) =
        let a = memory |> getParameter (state.InstructionPointer + 1) mode1
        let b = memory |> getParameter (state.InstructionPointer + 2) mode2
        let target = memory.[state.InstructionPointer + 3]

        memory |> setCodeAt target (op a b)

    let jump condition (mode1, mode2) =
        let test = memory |> getParameter (state.InstructionPointer + 1) mode1
        let target = memory |> getParameter (state.InstructionPointer + 2) mode2

        if test |> condition
        then target
        else state.InstructionPointer + 3

    let setConditionalValue op (mode1, mode2) =
        let param1 = memory |> getParameter (state.InstructionPointer + 1) mode1
        let param2 = memory |> getParameter (state.InstructionPointer + 2) mode2
        let target = memory.[state.InstructionPointer + 3]
        
        memory |> setCodeAt target (if op param1 param2 then 1 else 0)

    if state.IsHalted || state.IsPaused then
        return memory |> IntCodeMemory |> Some
    else
        match memory.[state.InstructionPointer] |> toInstruction with
        | Some { Code = Add; ParameterModes = [ mode1; mode2; _ ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4 }
            return performBinary (+) (mode1, mode2) |> IntCodeMemory |> Some
        | Some { Code = Multiply; ParameterModes = [ mode1; mode2; _ ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4 }
            return performBinary (*) (mode1, mode2) |> IntCodeMemory |> Some
        | Some { Code = Input; ParameterModes = [ _ ] } ->
            match state.Input with
            | input :: inputs ->
                let param = memory.[state.InstructionPointer + 1]
                do! State.put { state with InstructionPointer = state.InstructionPointer + 2; Input = inputs }
                return memory |> setCodeAt param input |> IntCodeMemory |> Some
            | [] ->
                do! State.put { state with IsPaused = true }
                return memory |> IntCodeMemory |> Some
        | Some { Code = Output; ParameterModes = [ mode ] } ->
            let output = memory |> getParameter (state.InstructionPointer + 1) mode
            do! State.put {
                state with InstructionPointer = state.InstructionPointer + 2; Output = state.Output @ [ output ]
            }
            return memory |> IntCodeMemory |> Some
        | Some { Code = JumpIfTrue; ParameterModes = [ mode1; mode2 ] } ->
            do! State.put { state with InstructionPointer = jump ((<>) 0) (mode1, mode2) }
            return memory |> IntCodeMemory |> Some
        | Some { Code = JumpIfFalse; ParameterModes = [ mode1; mode2 ] } ->
            do! State.put { state with InstructionPointer = jump ((=) 0) (mode1, mode2) }
            return memory |> IntCodeMemory |> Some
        | Some { Code = Less; ParameterModes = [ mode1; mode2; _ ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4 }
            return setConditionalValue (<) (mode1, mode2) |> IntCodeMemory |> Some
        | Some { Code = Equals; ParameterModes = [ mode1; mode2; _ ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4 }
            return setConditionalValue (=) (mode1, mode2) |> IntCodeMemory |> Some
        | Some { Code = Halt; ParameterModes = [] } ->
            do! State.put { state with IsHalted = true }
            return memory |> IntCodeMemory |> Some
        | _ ->
            return None
}

let rec exec memory = monad {
    match! execStep memory with
    | Some memory ->
        let! state = State.get
        if not (state.IsHalted || state.IsPaused)
        then return! exec memory
        else return Some memory
    | None ->
        return None
}

let execAndGetResult memory initialState =
    let memory, state = State.run (exec memory) initialState
    memory |> Option.map (fun memory -> memory, state)

let rec execPipeline states input =
    match states with
    | (memory, state) :: states when not state.IsHalted ->
        monad {
            let! (memory, state) =
                execAndGetResult memory { state with Input = state.Input @ [ input ]; IsPaused = false }
            let! output = state.Output |> List.tryExactlyOne
            let newState = memory, { state with Output = [] }
            return! execPipeline (states @ [ newState ]) output
        }
    | _ -> Some input

let strToInt (str : string) =
    let success, num = Int32.TryParse(str)
    if success then Some num else None

let rec distribute e =
    function
    | [] -> [ [ e ] ]
    | x :: xs' as xs -> (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

let rec permute =
    function
    | [] -> [ [] ]
    | e :: xs -> permute xs |> List.collect (distribute e)

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> List.collect (fun nums -> nums.Split(',') |> List.ofArray)
    |> List.map strToInt
    |> List.choose id
    |> List.toArray

let initialState input =
    { InstructionPointer = 0; Input = [ input ]; Output = []; IsPaused = false; IsHalted = false }
    
[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let input = getInput file |> IntCodeMemory

        [ 5..9 ]
        |> permute
        |> Seq.ofList
        |> Seq.zip (Seq.initInfinite (fun _ -> input))
        |> Seq.map (fun (memory, configs) -> configs |> List.map initialState |> List.map (fun state -> memory, state))
        |> Seq.map (fun states -> execPipeline states 0)
        |> Seq.choose id
        |> Seq.sortDescending
        |> Seq.tryHead
        |> Option.iter (printfn "%d")

        0
    | _ ->
        printfn "Usage: Aoc.Day07 file"
        1

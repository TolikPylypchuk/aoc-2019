open System
open System.Numerics
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
    | OffsetRelativeBase
    | Halt

type ParameterMode =
    | Position
    | Immediate
    | Relative

type Instruction = {
    Code : OpCode
    ParameterModes : ParameterMode list
}

type IntCodeMemory = IntCodeMemory of Map<bigint, bigint>

type IntCodeState = {
    InstructionPointer : bigint
    RelativeBase : bigint
    Input : bigint list
    Output : bigint list
    IsPaused : bool
    IsHalted : bool
}

type Item =
    | Antenna
    | WeatherMachine
    | KleinBottle
    | SpoolOfCat6
    | Mug
    | Shell
    | Tambourine
    | Cake

let opCode code =
    match int code with
    | 1 -> Some Add
    | 2 -> Some Multiply
    | 3 -> Some Input
    | 4 -> Some Output
    | 5 -> Some JumpIfTrue
    | 6 -> Some JumpIfFalse
    | 7 -> Some Less
    | 8 -> Some Equals
    | 9 -> Some OffsetRelativeBase
    | 99 -> Some Halt
    | _ -> None

let flippedDigits = List.unfold (fun num -> if num = (bigint 0) then None else Some (num % 10I, num / 10I))

let parameterMode mode =
    match int mode with
    | 0 -> Some Position
    | 1 -> Some Immediate
    | 2 -> Some Relative
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
    | OffsetRelativeBase -> 1
    | Halt -> 0

let toInstruction instruction = monad {
    let! code = instruction % 100I |> opCode
    let! modes =
        instruction / 100I
        |> flippedDigits
        |> List.map parameterMode
        |> sequence

    let expected = code |> numParameters
    let actual = modes |> List.length

    return { Code = code; ParameterModes = modes @ List.replicate (expected - actual) Position }
}

let get index (IntCodeMemory memory) =
    memory |> Map.tryFind index |> Option.defaultValue zero

let set relativeBase mode target newValue (IntCodeMemory memory) =
    let actualTarget =
        match mode with
        | Relative -> target + relativeBase
        | _ -> target

    memory |> Map.add actualTarget newValue |> IntCodeMemory

let getParameter relativeBase position mode memory =
    match mode with
    | Position -> memory |> get (memory |> get position)
    | Immediate -> memory |> get position
    | Relative -> memory |> get ((memory |> get position) + relativeBase)

let execStep memory = monad {
    let! state = State.get

    let getParameter = getParameter state.RelativeBase
    let set = set state.RelativeBase

    let performBinary mode op (mode1, mode2) =
        let a = memory |> getParameter (state.InstructionPointer + 1I) mode1
        let b = memory |> getParameter (state.InstructionPointer + 2I) mode2
        let target = memory |> get (state.InstructionPointer + 3I)

        memory |> set mode target (op a b)

    let jump condition (mode1, mode2) =
        let test = memory |> getParameter (state.InstructionPointer + 1I) mode1
        let target = memory |> getParameter (state.InstructionPointer + 2I) mode2

        if test |> condition
        then target
        else state.InstructionPointer + 3I

    let setConditionalValue mode op (mode1, mode2) =
        let param1 = memory |> getParameter (state.InstructionPointer + 1I) mode1
        let param2 = memory |> getParameter (state.InstructionPointer + 2I) mode2
        let target = memory |> get (state.InstructionPointer + 3I)

        memory |> set mode target (if op param1 param2 then one else zero)

    if state.IsHalted || state.IsPaused then
        return memory |> Some
    else
        let instruction = memory |> get state.InstructionPointer |> toInstruction
        match instruction with

        | Some { Code = Add; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4I }
            return performBinary mode (+) (mode1, mode2) |> Some

        | Some { Code = Multiply; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4I }
            return performBinary mode (*) (mode1, mode2) |> Some

        | Some { Code = Input; ParameterModes = [ mode ] } ->
            match state.Input with
            | input :: inputs ->
                let param = memory |> get (state.InstructionPointer + one)
                do! State.put { state with InstructionPointer = state.InstructionPointer + 2I; Input = inputs }
                return memory |> set mode param input |> Some
            | [] ->
                do! State.put { state with IsPaused = true }
                return memory |> Some

        | Some { Code = Output; ParameterModes = [ mode ] } ->
            let output = memory |> getParameter (state.InstructionPointer + one) mode
            do! State.put {
                state with InstructionPointer = state.InstructionPointer + 2I; Output = state.Output @ [ output ]
            }
            return memory |> Some

        | Some { Code = JumpIfTrue; ParameterModes = [ mode1; mode2 ] } ->
            do! State.put { state with InstructionPointer = jump ((<>) zero) (mode1, mode2) }
            return memory |> Some

        | Some { Code = JumpIfFalse; ParameterModes = [ mode1; mode2 ] } ->
            do! State.put { state with InstructionPointer = jump ((=) zero) (mode1, mode2) }
            return memory |> Some

        | Some { Code = Less; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4I }
            return setConditionalValue mode (<) (mode1, mode2) |> Some

        | Some { Code = Equals; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4I }
            return setConditionalValue mode (=) (mode1, mode2) |> Some

        | Some { Code = OffsetRelativeBase; ParameterModes = [ mode ] } ->
            let newPointer = state.InstructionPointer + 2I
            let newRelativeBase = state.RelativeBase + (memory |> getParameter (state.InstructionPointer + one) mode)
            do! State.put { state with InstructionPointer = newPointer; RelativeBase = newRelativeBase }
            return memory |> Some

        | Some { Code = Halt; ParameterModes = [] } ->
            do! State.put { state with IsHalted = true }
            return memory |> Some

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

let strToBigInt (str : string) =
    let success, num = BigInteger.TryParse(str)
    if success then Some num else None

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> List.collect (fun nums -> nums.Split(',') |> List.ofArray)
    |> List.map strToBigInt
    |> List.choose id
    |> List.mapi (fun index item -> (bigint index, item))
    |> Map.ofList
    |> IntCodeMemory

let initialState input = {
    InstructionPointer = zero
    RelativeBase = zero
    Input = input
    Output = []
    IsPaused = false
    IsHalted = false
}

let outputToString =
    List.map int
    >> List.map char
    >> List.toArray
    >> String

let inputToNumbers =
    Seq.toList
    >> List.map int
    >> List.map bigint

let play memory items =
    let input =
        [
            "east"

            if items |> List.contains Antenna then
                "take antenna"

            "west"
            "north"

            if items |> List.contains WeatherMachine then
                "take weather machine"

            "north"

            if items |> List.contains KleinBottle then
                "take klein bottle"

            "east"

            if items |> List.contains SpoolOfCat6 then
                "take spool of cat6"

            "east"
            "south"

            if items |> List.contains Mug then
                "take mug"

            "north"
            "north"
            "east"
            "south"

            if items |> List.contains Shell then
                "take shell"

            "north"
            "north"
            "north"

            if items |> List.contains Tambourine then
                "take tambourine"

            "south"
            "south"
            "west"
            "west"
            "north"

            if items |> List.contains Cake then
                "take cake"

            "south"
            "east"
            "south"
            "west"
            "south"
            "south"
            "east"
            "inv"
        ]
        |> List.map (sprintf "%s\n")
        |> List.collect inputToNumbers

    let state = State.exec (exec memory) (initialState input)

    let output = state.Output |> outputToString

    if output.Contains("Alert!")
    then None
    else Some output

let rec powerset =
   function
   | [] -> [ [] ]
   | x :: xs ->
      let xss = powerset xs
      List.map (fun xs' -> x :: xs') xss @ xss

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let memory = file |> getInput

        [ Antenna; WeatherMachine; KleinBottle; SpoolOfCat6; Mug; Shell; Tambourine; Cake ]
        |> powerset
        |> List.takeWhile (fun items ->
            let result = play memory items
            match result with
            | Some result ->
                printfn "%s" result
                false
            | None ->
                true)
        |> ignore

        0
    | _ ->
        printfn "Usage: Aoc.Day25 file"
        1

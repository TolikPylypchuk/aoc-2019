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

type TileType =
    | Empty
    | Wall
    | Block
    | Paddle
    | Ball

type Point = {
    X : int
    Y : int
}

type Tile = {
    Position : Point
    Type : TileType
}

type GameOutput = {
    Score : int
    Tiles : Tile list
}

type JoystickMove =
    | Neutral
    | Left
    | Right

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

let tileType code =
    match int code with
    | 0 -> Some Empty
    | 1 -> Some Wall
    | 2 -> Some Block
    | 3 -> Some Paddle
    | 4 -> Some Ball
    | _ -> None

let negativeOne = bigint -1
let zero = bigint 0
let one = bigint 1
let two = bigint 2
let three = bigint 3
let four = bigint 4
let ten = bigint 10
let oneHundred = bigint 100

let moveValue = function Neutral -> zero | Left -> negativeOne | Right -> one

let flippedDigits = List.unfold (fun num -> if num = (bigint 0) then None else Some (num % ten, num / ten))

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
    let! code = instruction % oneHundred |> opCode
    let! modes =
        instruction / oneHundred
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
        let a = memory |> getParameter (state.InstructionPointer + one) mode1
        let b = memory |> getParameter (state.InstructionPointer + two) mode2
        let target = memory |> get (state.InstructionPointer + three)

        memory |> set mode target (op a b)

    let jump condition (mode1, mode2) =
        let test = memory |> getParameter (state.InstructionPointer + one) mode1
        let target = memory |> getParameter (state.InstructionPointer + two) mode2

        if test |> condition
        then target
        else state.InstructionPointer + three

    let setConditionalValue mode op (mode1, mode2) =
        let param1 = memory |> getParameter (state.InstructionPointer + one) mode1
        let param2 = memory |> getParameter (state.InstructionPointer + two) mode2
        let target = memory |> get (state.InstructionPointer + three)
        
        memory |> set mode target (if op param1 param2 then one else zero)

    if state.IsHalted || state.IsPaused then
        return memory |> Some
    else
        let instruction = memory |> get state.InstructionPointer |> toInstruction
        match instruction with

        | Some { Code = Add; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + four }
            return performBinary mode (+) (mode1, mode2) |> Some

        | Some { Code = Multiply; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + four }
            return performBinary mode (*) (mode1, mode2) |> Some

        | Some { Code = Input; ParameterModes = [ mode ] } ->
            match state.Input with
            | input :: inputs ->
                let param = memory |> get (state.InstructionPointer + one)
                do! State.put { state with InstructionPointer = state.InstructionPointer + two; Input = inputs }
                return memory |> set mode param input |> Some
            | [] ->
                do! State.put { state with IsPaused = true }
                return memory |> Some

        | Some { Code = Output; ParameterModes = [ mode ] } ->
            let output = memory |> getParameter (state.InstructionPointer + one) mode
            do! State.put {
                state with InstructionPointer = state.InstructionPointer + two; Output = state.Output @ [ output ]
            }
            return memory |> Some

        | Some { Code = JumpIfTrue; ParameterModes = [ mode1; mode2 ] } ->
            do! State.put { state with InstructionPointer = jump ((<>) zero) (mode1, mode2) }
            return memory |> Some

        | Some { Code = JumpIfFalse; ParameterModes = [ mode1; mode2 ] } ->
            do! State.put { state with InstructionPointer = jump ((=) zero) (mode1, mode2) }
            return memory |> Some

        | Some { Code = Less; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + four }
            return setConditionalValue mode (<) (mode1, mode2) |> Some

        | Some { Code = Equals; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + four }
            return setConditionalValue mode (=) (mode1, mode2) |> Some

        | Some { Code = OffsetRelativeBase; ParameterModes = [ mode ] } ->
            let newPointer = state.InstructionPointer + two
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

let execAndGetResult initialState memory =
    let _, state = State.run (exec memory) initialState
    state.Output

let toSingleGameOutput output (x, y, num) =
    if x = -1 && y = 0 then
        { output with Score = num }
    else
        num
        |> tileType
        |>> fun type' -> { Position = { X = x; Y = y }; Type = type' }
        |>> fun tile -> { output with Tiles = tile :: output.Tiles }
        |> Option.defaultValue output

let toGameOutput nums =
    nums
    |> List.map int
    |> List.chunkBySize 3
    |> List.choose (function | [ a; b; c ] -> Some (a, b, c) | _ -> None)
    |> List.fold toSingleGameOutput { Score = 0; Tiles = [] }

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

let cheat (IntCodeMemory memory) =
    memory |> Map.add zero two |> IntCodeMemory

let initialState =
    { InstructionPointer = zero; RelativeBase = zero; Input = []; Output = []; IsPaused = false; IsHalted = false }

let findX type' tiles =
    tiles
    |> List.filter (fun tile -> tile.Type = type')
    |> List.map (fun tile -> tile.Position.X)
    |> List.tryExactlyOne
    |> Option.defaultValue 0

let rec playGame input memory state score =
    if not state.IsHalted then

        let memory, state = State.run (exec memory) { state with Input = input; Output = []; IsPaused = false }
        match memory with
        | Some memory ->
            let output = state.Output |> toGameOutput

            let ballX = output.Tiles |> findX Ball
            let paddleX = output.Tiles |> findX Paddle

            let nextMove =
                if ballX > paddleX
                then Right
                elif ballX < paddleX
                then Left
                else Neutral

            playGame [ nextMove |> moveValue ] memory state output.Score
        | None ->
            score
    else
        score

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let memory = getInput file |> IntCodeMemory

        memory
        |> execAndGetResult initialState
        |> toGameOutput
        |> fun output -> output.Tiles
        |> List.filter (fun tile -> tile.Type = Block)
        |> List.length
        |> printfn "%d"

        playGame [] (memory |> cheat) initialState 0
        |> printfn "%d"

        0
    | _ ->
        printfn "Usage: Aoc.Day13 file"
        1

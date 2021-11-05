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

type TileType =
    | Empty
    | Scaffold
    | RobotUp
    | RobotDown
    | RobotLeft
    | RobotRight
    | RobotTumbling

type Tile = {
    X : int
    Y : int
    Type : TileType
}

type Move =
    | TurnRight
    | TurnLeft
    | Forward of int

type Direction =
    | Up
    | Down
    | Left
    | Right

let parseTile =
    string >>
    function
    | "." -> Some Empty
    | "#" -> Some Scaffold
    | "^" -> Some RobotUp
    | "v" -> Some RobotDown
    | "<" -> Some RobotLeft
    | ">" -> Some RobotRight
    | "X" -> Some RobotTumbling
    | _ -> None

let isScaffold tile = match tile.Type with Empty | RobotTumbling -> false | _ -> true

let asCommandString =
    function
    | TurnLeft -> "L"
    | TurnRight -> "R"
    | Forward x -> string x

let zero = bigint 0
let one = bigint 1
let two = bigint 2
let three = bigint 3
let four = bigint 4
let ten = bigint 10
let oneHundred = bigint 100

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

let initialState = {
    InstructionPointer = zero
    RelativeBase = zero
    Input = []
    Output = []
    IsPaused = false
    IsHalted = false
}

let outputsToString =
    List.map int
    >> List.map char
    >> List.toArray
    >> String

let drawMap outputs =
    Console.Clear()

    outputs
    |> outputsToString
    |> printfn "%s"

let isIntersecion tiles tile =
    isScaffold tile &&
    tile.X > 0 &&
    tile.X < (tiles |> Array2D.length2) - 1 &&
    tile.Y > 0 &&
    tile.Y < (tiles |> Array2D.length1) - 1 &&
    isScaffold tiles.[tile.Y - 1, tile.X] &&
    isScaffold tiles.[tile.Y + 1, tile.X] &&
    isScaffold tiles.[tile.Y, tile.X - 1] &&
    isScaffold tiles.[tile.Y, tile.X + 1]

let toTiles =
    outputsToString
    >> String.split (seq { "\n" })
    >> Seq.mapi (fun y ->
        Seq.mapi (fun x ch -> ch |> parseTile |>> fun type' -> { X = x; Y = y; Type = type' })
        >> Seq.choose id
        >> Seq.toList)
    >> Seq.filter (not << List.isEmpty)
    >> array2D

let calculateAlignmentParameters tiles =
    tiles
    |> Seq.cast<Tile>
    |> Seq.filter (isIntersecion tiles)
    |> Seq.map (fun tile -> tile.X * tile.Y)
    |> Seq.sum

let tryFindNextScaffold (tiles : Tile[,]) direction currentTile =
    let tile =
        match direction with
        | Up when currentTile.Y > 0 -> Some tiles.[currentTile.Y - 1, currentTile.X]
        | Down when currentTile.Y < (tiles |> Array2D.length1) - 1 -> Some tiles.[currentTile.Y + 1, currentTile.X]
        | Left when currentTile.X > 0 -> Some tiles.[currentTile.Y, currentTile.X - 1]
        | Right when currentTile.X < (tiles |> Array2D.length2) - 1 -> Some tiles.[currentTile.Y, currentTile.X + 1]
        | _ -> None

    tile |> Option.filter (fun tile -> tile.Type = Scaffold)

let calculateInitialDirection (tiles : Tile[,]) tile =
    if tile.Y > 0 && tiles.[tile.Y - 1, tile.X].Type = Scaffold
    then Up
    elif tile.Y < (tiles |> Array2D.length1) - 1 && tiles.[tile.Y + 1, tile.X].Type = Scaffold
    then Down
    elif tile.X > 0 && tiles.[tile.Y, tile.X - 1].Type = Scaffold
    then Left
    else Right

let calculateNextDirection (tiles : Tile[,]) direction tile =
    if tile.Y > 0 && tiles.[tile.Y - 1, tile.X].Type = Scaffold && direction <> Down
    then Some Up
    elif tile.Y < (tiles |> Array2D.length1) - 1 && tiles.[tile.Y + 1, tile.X].Type = Scaffold && direction <> Up
    then Some Down
    elif tile.X > 0 && tiles.[tile.Y, tile.X - 1].Type = Scaffold && direction <> Right
    then Some Left
    elif tile.X < (tiles |> Array2D.length2) - 1 && tiles.[tile.Y, tile.X + 1].Type = Scaffold && direction <> Left
    then Some Right
    else None

let addMoveForward moves =
    match moves with
    | [] -> [ Forward 1 ]
    | Forward x :: moves -> Forward (x + 1) :: moves
    | _ -> Forward 1 :: moves

let addTurn previousDirection nextDirection moves =
    match previousDirection, nextDirection with
    | Up, Left -> TurnLeft :: moves
    | Up, Right -> TurnRight :: moves
    | Down, Left -> TurnRight :: moves
    | Down, Right -> TurnLeft :: moves
    | Left, Up -> TurnRight :: moves
    | Left, Down -> TurnLeft :: moves
    | Right, Up -> TurnLeft :: moves
    | Right, Down -> TurnRight :: moves
    | _ -> moves

let rec addMoves tiles previousMoves direction currentTile =
    match currentTile |> tryFindNextScaffold tiles direction with
    | Some tile ->
        addMoves tiles (previousMoves |> addMoveForward) direction tile
    | None ->
        match currentTile |> calculateNextDirection tiles direction with
        | Some newDirection -> addMoves tiles (previousMoves |> addTurn direction newDirection) newDirection currentTile
        | None -> previousMoves

let convertToMoves (tiles : Tile[,]) =
    let robotTile =
        tiles
        |> Seq.cast<Tile>
        |> Seq.filter (fun t -> match t.Type with RobotUp | RobotDown | RobotLeft | RobotRight -> true | _ -> false)
        |> Seq.head

    let direction = robotTile |> calculateInitialDirection tiles

    let firstMoves =
        match robotTile.Type, direction with
        | RobotUp, Down -> [ TurnLeft; TurnLeft ]
        | RobotUp, Left -> [ TurnLeft ]
        | RobotUp, Right -> [ TurnRight ]
        | RobotDown, Up -> [ TurnLeft; TurnLeft ]
        | RobotDown, Left -> [ TurnRight ]
        | RobotDown, Right -> [ TurnLeft ]
        | RobotLeft, Up -> [ TurnRight ]
        | RobotLeft, Down -> [ TurnLeft ]
        | RobotLeft, Right -> [ TurnLeft; TurnLeft ]
        | RobotRight, Up -> [ TurnLeft ]
        | RobotRight, Down -> [ TurnRight ]
        | RobotRight, Left -> [ TurnLeft; TurnLeft ]
        | _ -> []

    robotTile
    |> addMoves tiles firstMoves direction
    |> List.rev

let asCommands = List.map asCommandString >> String.concat ","

let rec removeNoneFromStart list = match list with None :: list -> list |> removeNoneFromStart | _ -> list

let rec removeAllSublists sublist index list =
    let newList = list |> List.removeManyAt index (sublist |> List.length) |> List.insertAt index None
    match newList |> List.tryFindSliceIndex sublist with
    | Some index -> newList |> removeAllSublists sublist index
    | None -> newList

let rec findRoutines previousRoutines currentLength moves =
    let moves = moves |> removeNoneFromStart

    if (previousRoutines |> List.length) = 3 then
        if moves |> List.isEmpty
        then previousRoutines |> List.rev |> Some
        else None
    else
        let currentRoutine = moves |> List.truncate currentLength
        if currentRoutine |> List.exists (function None -> true | _ -> false) then
            None
        else
            if (currentRoutine |> List.choose id |> asCommands |> String.length) > 20 then
                None
            else
                let strippedMoves = moves |> removeAllSublists currentRoutine 0

                match findRoutines ((currentRoutine |> List.choose id) :: previousRoutines) 2 strippedMoves with
                | Some routines ->
                    Some routines
                | None when currentLength < (moves |> List.length) ->
                    findRoutines previousRoutines (currentLength + 2) moves
                | None ->
                    None

let enableRobot (IntCodeMemory memory) =
    memory |> Map.add zero two |> IntCodeMemory

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let memory = file |> getInput

        let _, state = State.run (exec memory) initialState
        drawMap state.Output

        let tiles = state.Output |> toTiles

        tiles
        |> calculateAlignmentParameters
        |> printfn "%d"

        let moves = tiles |> convertToMoves

        let routines =
            moves
            |> List.map Some
            |> findRoutines [] 2

        match routines with
        | Some [ a; b; c ] ->
            let routineA = a |> asCommands
            let routineB = b |> asCommands
            let routineC = c |> asCommands

            let definition =
                moves
                |> asCommands
                |> String.replace routineA "A"
                |> String.replace routineB "B"
                |> String.replace routineC "C"

            let input =
                [ definition; routineA; routineB; routineC; "n" ]
                |> List.map (fun str -> $"{str}\n")
                |> String.concat ""
                |> Seq.map int
                |> Seq.map bigint
                |> Seq.toList
                
            let _, state = State.run (memory |> enableRobot |> exec) { initialState with Input = input }

            state.Output
            |> List.tryLast
            |> Option.iter (printfn ("%O"))
        | _ ->
            ()

        0
    | _ ->
        printfn "Usage: Aoc.Day17 file"
        1


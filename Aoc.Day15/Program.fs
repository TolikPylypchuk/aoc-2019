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

type Point = {
    X : int
    Y : int
}

type Direction =
    | North
    | South
    | West
    | East

type MoveResult =
    | Moved
    | HitWall
    | FoundOxygenSystem

type Droid = {
    Location : Point
    CurrentDirection : Direction
    Walls : Set<Point>
    VisitedLocations : Point list
}

type DrawablePointType =
    | Wall
    | PreviousLocation
    | DroidNorth
    | DroidSouth
    | DroidWest
    | DroidEast

type DrawablePoint = {
    Point : Point
    Type : DrawablePointType
}

let zero = bigint 0
let one = bigint 1
let two = bigint 2
let three = bigint 3
let four = bigint 4
let ten = bigint 10
let oneHundred = bigint 100

let directionValue =
    function
    | North -> one
    | South -> two
    | West -> three
    | East -> four

let move droid =
    let location = droid.Location
    let newLocation =
        match droid.CurrentDirection with
        | North -> { location with Y = location.Y + 1 }
        | South -> { location with Y = location.Y - 1 }
        | West -> { location with X = location.X - 1 }
        | East -> { location with X = location.X + 1 }
    { droid with Location = newLocation }
    
let memorizeLocation droid =
    { droid with VisitedLocations = droid.VisitedLocations @ [ droid.Location ] }
        
let memorizeWallAhead droid =
    { droid with Walls = droid.Walls |> Set.add (droid |> move).Location }

let turnRight droid =
    let newDirection =
        match droid.CurrentDirection with
        | North -> East
        | South -> West
        | West -> North
        | East -> South
    { droid with CurrentDirection = newDirection }

let turnLeft droid =
    let newDirection =
        match droid.CurrentDirection with
        | North -> West
        | South -> East
        | West -> South
        | East -> North
    { droid with CurrentDirection = newDirection }

let moveResult =
    function
    | num when num = zero -> Some HitWall
    | num when num = one -> Some Moved
    | num when num = two -> Some FoundOxygenSystem
    | _ -> None

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

let putState newState = monad {
    let! _, droid = State.get
    do! State.put (newState, droid)
}

let putDroid newDroid = monad {
    let! state, _ = State.get
    do! State.put (state, newDroid)
}

let execStep memory = monad {
    let! state, _ = State.get

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
            do! putState { state with InstructionPointer = state.InstructionPointer + four }
            return performBinary mode (+) (mode1, mode2) |> Some

        | Some { Code = Multiply; ParameterModes = [ mode1; mode2; mode ] } ->
            do! putState { state with InstructionPointer = state.InstructionPointer + four }
            return performBinary mode (*) (mode1, mode2) |> Some

        | Some { Code = Input; ParameterModes = [ mode ] } ->
            match state.Input with
            | input :: inputs ->
                let param = memory |> get (state.InstructionPointer + one)
                do! putState { state with InstructionPointer = state.InstructionPointer + two; Input = inputs }
                return memory |> set mode param input |> Some
            | [] ->
                do! putState { state with IsPaused = true }
                return memory |> Some

        | Some { Code = Output; ParameterModes = [ mode ] } ->
            let output = memory |> getParameter (state.InstructionPointer + one) mode
            do! putState {
                state with InstructionPointer = state.InstructionPointer + two; Output = state.Output @ [ output ]
            }
            return memory |> Some

        | Some { Code = JumpIfTrue; ParameterModes = [ mode1; mode2 ] } ->
            do! putState { state with InstructionPointer = jump ((<>) zero) (mode1, mode2) }
            return memory |> Some

        | Some { Code = JumpIfFalse; ParameterModes = [ mode1; mode2 ] } ->
            do! putState { state with InstructionPointer = jump ((=) zero) (mode1, mode2) }
            return memory |> Some

        | Some { Code = Less; ParameterModes = [ mode1; mode2; mode ] } ->
            do! putState { state with InstructionPointer = state.InstructionPointer + four }
            return setConditionalValue mode (<) (mode1, mode2) |> Some

        | Some { Code = Equals; ParameterModes = [ mode1; mode2; mode ] } ->
            do! putState { state with InstructionPointer = state.InstructionPointer + four }
            return setConditionalValue mode (=) (mode1, mode2) |> Some

        | Some { Code = OffsetRelativeBase; ParameterModes = [ mode ] } ->
            let newPointer = state.InstructionPointer + two
            let newRelativeBase = state.RelativeBase + (memory |> getParameter (state.InstructionPointer + one) mode)
            do! putState { state with InstructionPointer = newPointer; RelativeBase = newRelativeBase }
            return memory |> Some

        | Some { Code = Halt; ParameterModes = [] } ->
            do! putState { state with IsHalted = true }
            return memory |> Some

        | _ ->
            return None
}

let rec exec memory = monad {
    match! execStep memory with
    | Some memory ->
        let! state, _ = State.get
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

let initialState = {
    InstructionPointer = zero
    RelativeBase = zero
    Input = [ North |> directionValue ]
    Output = []
    IsPaused = false
    IsHalted = false
}

let initialDroid =
    { Location = { X = 0; Y = 0 }; CurrentDirection = North; Walls = Set.empty; VisitedLocations = [] }

let drawPoint drawable =
    match drawable.Type with
    | Wall -> "█"
    | PreviousLocation -> " "
    | DroidNorth -> "^"
    | DroidSouth -> "v"
    | DroidWest -> "<"
    | DroidEast -> ">"

let drawRow row =
    let lastColumn = row |> List.maxBy (fun drawable -> drawable.Point.X)
    let columns = row |> List.map (fun drawable -> drawable.Point.X, drawable) |> Map.ofList
    [ for col in 0 .. lastColumn.Point.X -> columns |> Map.tryFind col |>> drawPoint |> Option.defaultValue " " ]
    |> String.concat ""

let asDrawable minX type' points =
    points
    |> List.map (fun point -> { point with X = point.X - minX })
    |> List.map (fun point -> { Point = point; Type = type' })

let asDrawableDroid minX droid =
    let drawableType =
        match droid.CurrentDirection with
        | North -> DroidNorth
        | South -> DroidSouth
        | West -> DroidWest
        | East -> DroidEast
    droid.Location |> List.singleton |> asDrawable minX drawableType

let drawMap droid =
    Console.Clear()

    let minXWall = droid.Walls |> Set.map (fun point -> point.X) |> Set.toList |> List.tryHead
    let minXLocation = droid.VisitedLocations |> List.map (fun point -> point.X) |> List.tryHead

    let minX =
        match minXWall, minXLocation with
        | Some wall, Some loc -> minimum [ wall; loc; droid.Location.X ]
        | Some wall, None -> min wall droid.Location.X
        | None, Some loc -> min loc droid.Location.X
        | None, None -> droid.Location.X

    let drawableWalls = droid.Walls |> Set.toList |> asDrawable minX Wall
    let drawablePreviousLocations = droid.VisitedLocations |> asDrawable minX PreviousLocation
    let drawableDroid = droid |> asDrawableDroid minX

    let rows =
        List.concat [ drawableWalls; drawablePreviousLocations; drawableDroid ]
        |> List.groupBy (fun drawable -> drawable.Point.Y)

    let rowSet = rows |> List.map fst |> Set.ofList

    let firstRowNumber = rowSet |> Set.maxElement
    let lastRowNumber = rowSet |> Set.minElement

    let rowMap = rows |> Map.ofList

    [ for row in lastRowNumber .. firstRowNumber -> rowMap |> Map.tryFind row |>> drawRow |> Option.defaultValue "" ]
    |> List.rev
    |> String.concat "\n"
    |> printfn "%s"

let rec findOxygenSystem memory (state, droid) tryToTurn found = monad {
    let hypotheticalDroid = droid |> turnLeft |> move
    if (not tryToTurn) || (hypotheticalDroid.Walls |> Set.contains hypotheticalDroid.Location) then
        let state = { state with Input = [ droid.CurrentDirection |> directionValue ]; IsPaused = false }
        let memory, (state, droid) = State.run (exec memory) (state, droid)
        let! memory = memory

        let! result = state.Output |> List.tryExactlyOne >>= moveResult

        match result with
        | Moved ->
            let droid = droid |> memorizeLocation |> move
            let state =
                { state with Input = [ droid.CurrentDirection |> directionValue ]; Output = []; IsPaused = false }
            return! findOxygenSystem memory (state, droid) true found
        | HitWall ->
            let droid = droid |> memorizeWallAhead |> turnRight
            let state =
                { state with Input = [ droid.CurrentDirection |> directionValue ]; Output = []; IsPaused = false }
            return! findOxygenSystem memory (state, droid) true found
        | FoundOxygenSystem when found = false ->
            let droid = droid |> memorizeLocation |> move
            let state =
                { state with Input = [ droid.CurrentDirection |> directionValue ]; Output = []; IsPaused = false }
            return! findOxygenSystem memory (state, droid) true true
        | FoundOxygenSystem ->
            return droid
    else
        return! findOxygenSystem memory (state, droid |> turnLeft) false found
}

let rec findShortestPath visitedPoints result =
    match visitedPoints with
    | point :: points ->
        match points |> List.tryFindIndexBack ((=) point) with
        | None -> findShortestPath points (point :: result)
        | Some index -> findShortestPath (points |> List.removeManyAt 0 index) result
    | [] ->
        result

let rec fillOxygen allPoints oxydizedPoints (recentlyOxydizedPoints : Set<Point>) currentStep =
    let newOxydizedPoints =
        recentlyOxydizedPoints
        |> Set.toList
        |> List.collect (fun point -> [
            { point with X = point.X + 1 }
            { point with X = point.X - 1 }
            { point with Y = point.Y + 1 }
            { point with Y = point.Y - 1 }
        ])
        |> List.filter (fun point -> allPoints |> Set.contains point)
        |> List.filter (fun point -> oxydizedPoints |> Set.contains point |> not)
        |> Set.ofList

    if newOxydizedPoints |> Set.isEmpty
    then currentStep
    else fillOxygen allPoints (Set.union oxydizedPoints newOxydizedPoints) newOxydizedPoints (currentStep + 1)

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let memory = getInput file |> IntCodeMemory

        let droid = findOxygenSystem memory (initialState, initialDroid) true false

        match droid with
        | Some droid ->
            drawMap droid
            printfn "%d" ((findShortestPath droid.VisitedLocations [] |> List.length) + 1)

            let currentLocation = Set.singleton droid.Location
            printfn "%d" (fillOxygen (droid.VisitedLocations |> Set.ofList) currentLocation currentLocation 1)
        | None ->
            ()

        0
    | _ ->
        printfn "Usage: Aoc.Day15 file"
        1

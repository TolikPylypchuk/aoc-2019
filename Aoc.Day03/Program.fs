open System
open System.IO
open System.Text.RegularExpressions

type Direction =
    | Left
    | Right
    | Up
    | Down

type DirectedLength = {
    Direction : Direction
    Length : int
}

type Point = {
    X : int
    Y : int
}

type WireCreationState = {
    Start : Point
    Wire : Set<Point>
}

type Wire = Wire of Set<Point>

let (|Int|_|) (str : string) =
    let success, num = Int32.TryParse(str)
    if success then Some num else None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let parseDirectedLength str =
    match str with
    | Regex "L(\\d+)" [ Int num ] -> Some { Direction = Left; Length = num }
    | Regex "R(\\d+)" [ Int num ] -> Some { Direction = Right; Length = num }
    | Regex "U(\\d+)" [ Int num ] -> Some { Direction = Up; Length = num }
    | Regex "D(\\d+)" [ Int num ] -> Some { Direction = Down; Length = num }
    | _ -> None

let move directedLength point =
    match directedLength.Direction with
    | Left -> { point with X = point.X - directedLength.Length }
    | Right -> { point with X = point.X + directedLength.Length }
    | Up -> { point with Y = point.Y + directedLength.Length }
    | Down -> { point with Y = point.Y - directedLength.Length }

let generatePoints start directedLength =
    let finish = start |> move directedLength

    let generatePoints' start directedLength =
        let oneStep = { Direction = directedLength.Direction; Length = 1 }

        (start, false)
        |> List.unfold (fun (point, finished) ->
            if finished then
                None
            else
                let newPoint = point |> move oneStep
                Some (newPoint, (newPoint, newPoint = finish)))

    finish, generatePoints' start directedLength

let createWire directedLengths =
    let addFromDescriptions state directedLength =
        let finish, newPoints = generatePoints state.Start directedLength

        { Start = finish; Wire = state.Wire |> Set.union (newPoints |> Set.ofList) }

    directedLengths
    |> List.fold addFromDescriptions { Start = { X = 0; Y = 0 }; Wire = Set.empty }
    |> fun state -> state.Wire

let shortestDistance directedLengths point =
    let generatePoints' (start, points) directedLength =
        let finish, newPoints = generatePoints start directedLength
        finish, points @ newPoints

    directedLengths
    |> List.map (List.fold generatePoints' ({ X = 0; Y = 0 }, []) >> snd)
    |> List.sumBy (List.takeWhile ((<>) point) >> List.length >> (+) 1)

let manhattanDistanceFromStart point =
    (abs point.X) + (abs point.Y)

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> List.map (fun line -> line.Split(',') |> List.ofArray |> List.map parseDirectedLength |> List.choose id)

let tryFindMinIntersection min wires =
    wires
    |> List.reduce Set.intersect
    |> Set.toList
    |> List.map min
    |> List.sort
    |> List.tryHead
    
[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let input = getInput file
        let wires = input |> List.map createWire

        match input with
        | [] ->
            printfn "Error - invalid input"
        | _ ->
            let result1 = wires |> tryFindMinIntersection manhattanDistanceFromStart

            match result1 with
            | Some result ->
                printfn "%d" result
            | None ->
                printfn "Error - no intersections"
                
            let result2 = wires |> tryFindMinIntersection (shortestDistance input)

            match result2 with
            | Some result ->
                printfn "%d" result
            | None ->
                printfn "Error - no intersections"
        0
    | _ ->
        printfn "Usage: Aoc.Day03 file"
        1

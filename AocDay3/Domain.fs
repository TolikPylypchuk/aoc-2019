namespace AocDay3

open System

type Point = {
    X : int
    Y : int
}

type Direction = Left | Right | Up | Down

type WirePart = {
    Direction : Direction
    Length : int
}

type Wire = {
    Points : Point list
}

[<AutoOpen>]
module Util =
    
    let stringToInt (str : string) =
        let (success, value) = Int32.TryParse str
        if success then Some value else None

    let positive num = if num > 0 then Some num else None

    let split (ch : char) (str : string) = str.Split(ch) |> Array.toList
    
    let mergeMaps f =
        Map.fold (fun map key value ->
            match map |> Map.tryFind key with
            | Some value' -> map |> Map.add key (f value value')
            | None -> map |> Map.add key value)

module Direction =

    let ofChar =
        function
        | 'L' -> Some Left
        | 'R' -> Some Right
        | 'U' -> Some Up
        | 'D' -> Some Down
        | _ -> None

    let movePoint point =
        function
        | Left -> { point with X = point.X - 1 }
        | Right -> { point with X = point.X + 1 }
        | Up -> { point with Y = point.Y + 1 }
        | Down -> { point with Y = point.Y - 1 }

module WirePart =

    let ofString (str : string) =
        if str.Length < 2 then
            None
        else maybe {
            let! dir = Direction.ofChar str.[0]
            let! len = str.[1..]  |> stringToInt
            let! posLen = positive len
            return { Direction = dir; Length = posLen }
        }

module Wire =

    let private movePoint point part = Direction.movePoint point part.Direction

    let ofParts parts =
        let add part wire = state {
            let! origin = State.get

            let newPoints =
                (origin, part)
                |> List.unfold (fun (point, part) ->
                    if part.Length = 0 then
                        None
                    else
                        let newPoint = movePoint point part
                        Some (newPoint, (newPoint, { part with Length = part.Length - 1 })))

            do! newPoints |> List.tryLast |> Option.defaultValue origin |> State.put

            return { Points = List.append wire.Points newPoints }
        }

        parts
        |> List.fold (fun state part -> state |> State.bind (add part)) (State.retn { Points = List.empty })
        |> State.eval { X = 0; Y = 0 }

    let findIntersections wires =
        wires
        |> List.map (fun wire -> wire.Points |> Set.ofList)
        |> Set.intersectMany
        |> Set.toList

    let stepsToIntersections intersections wire =
        intersections
        |> List.map (fun intersection -> intersection, (wire.Points |> List.findIndex ((=) intersection)) + 1)
        |> Map.ofList

    let sumsOfStepsToIntersections wires =
        let intersections = findIntersections wires
        wires
        |> List.map (stepsToIntersections intersections)
        |> List.fold (mergeMaps (+)) Map.empty

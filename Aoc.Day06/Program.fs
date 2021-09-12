open System.IO

let rec countOrbits map item =
    map
    |> Map.tryFind item
    |> Option.map (countOrbits map)
    |> Option.map ((+) 1)
    |> Option.defaultValue 0

let countAllOrbits map =
    map
    |> Map.toList
    |> List.map fst
    |> List.sumBy (countOrbits map)

let getAllAncestors map =
    List.unfold (fun item -> map |> Map.tryFind item |> Option.map (fun item -> (item, item)))

let tryFindFirstCommonItem items1 items2 =
    items1
    |> Seq.map (fun item -> items2 |> Seq.tryFind ((=) item))
    |> Seq.choose id
    |> Seq.tryHead

let calculateOrbitalTransfers map =
    let youAncestors = "YOU" |> getAllAncestors map
    let sanAncestors = "SAN" |> getAllAncestors map

    tryFindFirstCommonItem youAncestors sanAncestors
    |> Option.map (fun ancestor ->
        let length1 = youAncestors |> List.takeWhile ((<>) ancestor) |> List.length
        let length2 = sanAncestors |> List.takeWhile ((<>) ancestor) |> List.length
        length1 + length2)
    |> Option.defaultValue (-1)

let getInput file =
    file
    |> File.ReadAllLines
    |> Array.map (fun str -> str.Split(')') |> function [| big; small |] -> Some (small, big) | _ -> None)
    |> Array.choose id
    |> Map.ofArray

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let input = getInput file

        printfn "%d" (countAllOrbits input)
        printfn "%d" (calculateOrbitalTransfers input)

        0
    | _ ->
        printfn "Usage: Aoc.Day06 file"
        1

open System
open System.IO

let tryInt (ch : char) =
    let success, num = Int32.TryParse(string ch)
    if success then Some num else None

let parseNumbers (nums : string) =
    nums
    |> Seq.choose tryInt
    |> Seq.toList

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> function [ line1 ] -> Some <| parseNumbers line1 | _ -> None

let numberOfDigit digit list =
    list |> List.filter ((=) digit) |> List.length

let calculateCheckNumber width height nums =
    nums
    |> List.chunkBySize (width * height)
    |> List.minBy (numberOfDigit 0)
    |> fun nums -> (nums |> numberOfDigit 1) * (nums |> numberOfDigit 2)

let decode width height nums =
    let size = width * height
    nums
    |> List.chunkBySize size
    |> List.fold (fun acc list -> List.zip acc list |> List.map (fun (a, b) -> b :: a)) (List.replicate size [])
    |> List.map (List.tryFindBack ((<>) 2))
    |> fun a -> a
    |> List.choose id
    |> List.chunkBySize width

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        match getInput file with
        | Some nums ->
            let width = 25
            let height = 6

            nums
            |> calculateCheckNumber width height
            |> printfn "%d"

            let draw = function 1 -> "I" | _ -> " "

            nums
            |> decode width height
            |> List.iter (fun nums -> nums |> List.fold (fun acc num -> acc + draw num) "" |> printfn "%s")
        | None ->
            printfn "Error - cannot parse input"
        0
    | _ ->
        printfn "Usage: Aoc.Day08 file"
        1

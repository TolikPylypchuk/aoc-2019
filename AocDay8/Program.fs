module AocDay8.Program

open System
open System.IO

let charToInt ch =
    let (success, value) = Int32.TryParse (string ch)
    if success then Some value else None

let numberOf digit = List.filter ((=) digit) >> List.length

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "The input file hasn't been specified"
    else
        let width = 25
        let height = 6

        let layers =
            argv.[0]
            |> File.ReadAllText
            |> List.ofSeq
            |> List.map charToInt
            |> List.choose id
            |> List.chunkBySize (width * height)

        layers
        |> List.minBy (numberOf 0)
        |> fun digits -> (digits |> numberOf 1) * (digits |> numberOf 2)
        |> printfn "%i"

        layers
        |> List.transpose
        |> List.map (List.tryFind ((<>) 2))
        |> List.choose id
        |> List.map string
        |> List.fold (+) ""
        |> printfn "%s"
    0

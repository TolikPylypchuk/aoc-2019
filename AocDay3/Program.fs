module AocDay3.Program

open System.IO

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "The input file hasn't been specified"
    else
        argv.[0]
        |> File.ReadLines
        |> Seq.toList
        |> List.map (split ',' >> List.map WirePart.ofString >> List.choose id >> Wire.ofParts)
        |> Wire.sumsOfStepsToIntersections
        |> Map.toList
        |> List.map snd
        |> List.min
        |> printfn "%i"
    0

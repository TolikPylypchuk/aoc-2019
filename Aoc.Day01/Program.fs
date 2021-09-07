open System
open System.IO

let strToInt (str : string) =
    let success, num = Int32.TryParse(str)
    if success then Some num else None

let fuel1 mass =
    mass / 3 - 2
 
let fuel2 mass =
    mass
    |> List.unfold (fun mass -> let fuel = fuel1 mass in if fuel < 0 then None else Some (fuel, fuel))
    |> List.sum

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> List.map strToInt
    |> List.choose id
    
[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let input = getInput file
        printfn "%d" (input |> List.sumBy fuel1)
        printfn "%d" (input |> List.sumBy fuel2)
        0
    | _ ->
        printfn "Usage: Aoc.Day01 file"
        1

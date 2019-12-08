module AocDay2.Program

open System
open System.IO

let stringToInt (str : string) =
    let (success, value) = Int32.TryParse str
    if success then Some value else None

let execute nums =
    let rec execute' index nums =
        let canExecute = index < (nums |> Array.length) - 3

        let transform op =
            let newNum = op nums.[nums.[index + 1]] nums.[nums.[index + 2]]
            let location = nums.[index + 3]
            nums |> Array.mapi (fun i num -> if i = location then newNum else num) |> execute' (index + 4)

        match nums.[index] with
        | 1 when canExecute -> transform (+)
        | 2 when canExecute -> transform (*)
        | 99 -> Result.Ok nums
        | num when canExecute -> num |> sprintf "%i is not a valid command" |> Result.Error
        | _ -> "The input has invalid length" |> Result.Error

    execute' 0 nums

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "The input file hasn't been specified"
    else
        let input =
            argv.[0]
            |> File.ReadAllText
            |> (fun str -> str.Split(','))
            |> Array.map stringToInt
            |> Array.choose id

        List.allPairs [1..99] [1..99]
        |> List.find (fun (a, b) ->
            input
            |> Array.mapi (fun index value -> match index with 1 -> a | 2 -> b | _ -> value)
            |> execute
            |> Result.map Array.head
            |> function Ok num -> num = 19690720 | _ -> false)
        |> fun (a, b) -> a * 100 + b
        |> printfn "%i"

    0

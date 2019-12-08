module AocDay4.Program

open System
open System.IO

let stringToInt (str : string) =
    let (success, value) = Int32.TryParse str
    if success then Some value else None

let trim (str : string) = str.Trim()

let split (ch : char) (str : string) = str.Split(ch) |> Array.toList

let exactlyTwo = function [a; b] -> Some (a, b) | _ -> None
    
let exactlyFour = function [a; b; c; d] -> Some (a, b, c, d) | _ -> None
    
let flattenOptionTuple =
    function
    | Some (Some x, Some y) -> (x, y)
    | _ -> (0, 0)

let canBePassword num =
    let digits =
        num
        |> List.unfold (fun x -> if x = 0 then None else Some (x % 10, x / 10))
        |> List.rev

    let windowedDigits =
        seq {
            yield -1
            yield! digits
            yield 10
        }
        |> Seq.toList
        |> List.windowed 4
        |> List.map exactlyFour
        |> List.choose id

    digits |> List.length = 6 &&
    windowedDigits |> List.exists (fun (a, b, c, d) -> b = c && a <> b && c <> d) &&
    windowedDigits |> List.forall (fun (a, b, c, d) -> a <= b && b <= c && c <= d)

let generatePasswordChoices (start, end') = seq {
    for num in start .. end' do if num |> canBePassword then yield num
}

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "The input file hasn't been specified"
    else
        argv.[0]
        |> File.ReadAllText
        |> trim
        |> split '-'
        |> List.map stringToInt
        |> exactlyTwo
        |> flattenOptionTuple
        |> generatePasswordChoices
        |> Seq.length
        |> printfn "%i"
    0

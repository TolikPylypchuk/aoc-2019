open System
open System.IO
open System.Text.RegularExpressions

let (|Int|_|) (str : string) =
    let success, num = Int32.TryParse(str)
    if success then Some num else None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let parseRange =
    function
    | Regex "(\\d+)-(\\d+)" [ Int start; Int end' ] -> Some (start, end')
    | _ -> None

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> function [ line1 ] -> parseRange line1 | _ -> None

let digits = List.unfold (fun num -> if num = 0 then None else Some (num % 10, num / 10)) >> List.rev

let isPasswordValid1 =
    digits
    >> List.fold (fun (lastDigit, isMonotonic, hadDouble) digit ->
        (digit, isMonotonic && digit >= lastDigit, hadDouble || digit = lastDigit))
        (0, true, false)
    >> fun (_, isMonotonic, hadDouble) -> isMonotonic && hadDouble

let isPasswordValid2 =
    digits
    >> List.fold (fun (lastDigit, isMonotonic, hadDouble, numDuplicates) digit ->
        let isMonotonic = isMonotonic && digit >= lastDigit
        let hadDouble = hadDouble || numDuplicates = 2 && digit <> lastDigit
        let numDuplicates = if digit <> lastDigit then 1 else numDuplicates + 1
        (digit, isMonotonic, hadDouble, numDuplicates))
        (0, true, false, 0)
    >> fun (_, isMonotonic, hadDouble, numDuplicates) -> isMonotonic && (hadDouble || numDuplicates = 2)

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        match getInput file with
        | Some (start, end') ->
            let nums = [ start .. end' ]

            nums
            |> List.filter isPasswordValid1
            |> List.length
            |> printfn "%d"
            
            nums
            |> List.filter isPasswordValid2
            |> List.length
            |> printfn "%d"
        | None ->
            printfn "Error - cannot parse input"
        0
    | _ ->
        printfn "Usage: Aoc.Day04 file"
        1

open System.IO

let toDigit =
    function
    | '0' -> Some 0
    | '1' -> Some 1
    | '2' -> Some 2
    | '3' -> Some 3
    | '4' -> Some 4
    | '5' -> Some 5
    | '6' -> Some 6
    | '7' -> Some 7
    | '8' -> Some 8
    | '9' -> Some 9
    | _ -> None

let generateCircularSeq lst = 
    let rec next () = 
        seq {
            for element in lst do
                yield element
            yield! next ()
        }

    next ()

let createPattern num =
    List.concat [ List.replicate num 0; List.replicate num 1; List.replicate num 0; List.replicate num -1 ]
    |> generateCircularSeq
    |> Seq.tail

let calculateSingle phase input =
    input
    |> Seq.zip (createPattern phase)
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.sum
    |> fun result -> (abs result) % 10

let calculate input =
    [ for num in 1 .. (input |> List.length) -> calculateSingle num input ]

let rec calculateMultiple currentPhase phases input =
    if currentPhase > phases
    then input
    else calculateMultiple (currentPhase + 1) phases (input |> calculate)

let calculateFast input =
    0 |> List.scanBack (fun d s -> (d + s) % 10) input

let rec calculateMultipleFast currentPhase phases input =
    if currentPhase > phases
    then input
    else calculateMultipleFast (currentPhase + 1) phases (input |> calculateFast)

let getOffset input =
    input
    |> List.take 7
    |> List.rev
    |> List.mapi (fun index item -> item * (pown 10 index))
    |> List.sum

let getInput =
    File.ReadAllText
    >> fun text -> text.ToCharArray()
    >> Array.toList
    >> List.choose toDigit

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let input = file |> getInput

        input
        |> calculateMultiple 1 2
        |> List.take 8
        |> List.map string
        |> String.concat ""
        |> printfn "%s"

        input
        |> List.replicate 10000
        |> List.collect id
        |> List.skip (getOffset input)
        |> calculateMultipleFast 1 100
        |> List.take 8
        |> List.map string
        |> String.concat ""
        |> printfn "%s"

        0
    | _ ->
        printfn "Usage: Aoc.Day16 file"
        1

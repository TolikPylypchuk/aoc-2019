open System
open System.IO
open System.Numerics
open System.Text.RegularExpressions

open FSharpPlus.Math.Generic

type AffineTransformation = {
    Scale : bigint
    Shift : bigint
}

let (|Int|_|) (str : string) =
    let success, num = BigInteger.TryParse(str)
    if success then Some num else None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let expMod a b n =
    let rec expMod' a b c =
        if b = 0I
        then c
        else expMod' (a * a % n) (b >>> 1) (if b &&& 1I = 0I then c else c * a % n)
    expMod' a b 1I

let tryParseTransfomation =
    function
    | Regex "deal into new stack" [] -> Some { Scale = -1I; Shift = -1I }
    | Regex "cut (-?\\d+)" [ Int num ] -> Some { Scale = 1I; Shift = -num }
    | Regex "deal with increment (\\d+)" [ Int num ] -> Some { Scale = num; Shift = 0I }
    | _ -> None

let getInput file =
    file
    |> File.ReadAllLines
    |> Array.toList
    |> List.choose tryParseTransfomation

let transform transformation length num =
    remE ((remE transformation.Scale length) * num + (remE transformation.Shift length)) length

let compose length { Scale = scale1; Shift = shift1 } { Scale = scale2; Shift = shift2 } =
    { Scale = remE (scale1 * scale2) length; Shift = remE (remE (scale2 * shift1) length + shift2) length }

let empty =
    { Scale = 1I; Shift = 0I }

let invert length transformation =
    let newScale = expMod transformation.Scale (length - 2I) length
    { Scale = newScale; Shift = (remE (-1I * newScale * transformation.Shift) length) }

let fold length =
    List.fold (compose length) empty

let rec composeMultiple length count transformation =
    if count < 2I then
        transformation
    elif count = 2I then
        compose length transformation transformation
    else
        let result = composeMultiple length (count / 2I) (compose length transformation transformation)

        if count % 2I = 0I
        then result
        else compose length result transformation

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let transformations = file |> getInput

        let length = 10007I
        let finalTransformation = transformations |> fold length
        let result = 2019I |> transform finalTransformation length
        printfn "%A" result

        let length = 119315717514047I
        let finalTransformation = transformations |> fold length |> composeMultiple length 101741582076661I
        let result = 2020I |> transform (invert length finalTransformation) length
        printfn "%A" result

        0
    | _ ->
        printfn "Usage: Aoc.Day22 file"
        1

module AocDay1.Program

open System
open System.IO

let flip f = fun a b -> f b a

let stringToInt (str : string) =
    let (success, value) = Int32.TryParse str
    if success then Some value else None

let positive input = if input > 0 then Some input else None

let pair a = (a, a)

let getFuel mass = mass / 3 - 2

let getTotalFuel = Seq.unfold (getFuel >> positive >> Option.map pair) >> Seq.sum

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "The input file hasn't been specified"
    else
        argv.[0]
        |> File.ReadLines
        |> Seq.map stringToInt
        |> Seq.choose id
        |> Seq.sumBy getTotalFuel
        |> printfn "%i"
    0

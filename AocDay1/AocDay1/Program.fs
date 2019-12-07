open System.IO

let flip f = fun a b -> f b a

let positive input = if input > 0 then Some input else None

let pair a = (a, a)

let getFuel = flip (/) 3 >> flip (-) 2 >> int

let getTotalFuel = Seq.unfold (getFuel >> positive >> Option.map pair) >> Seq.sum

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "The input file hasn't been specified"
    else
        argv.[0]
        |> File.ReadLines
        |> Seq.map int
        |> Seq.sumBy getTotalFuel
        |> printfn "%i"
    0

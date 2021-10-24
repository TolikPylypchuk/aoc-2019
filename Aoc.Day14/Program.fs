open System.IO

open FParsec

type Chemical = {
    Type : string
    Quantity : int64
}

type Reaction = {
    Input : Chemical list
    Output : Chemical
}

type ReactionChainState = {
    UsedOre : int64
    LeftoverChemicals : Map<string, int64>
}

let charsToString =
    List.map string >> List.fold (+) ""

let pChemical =
    spaces >>. pint64 .>>. (spaces1 >>. (many1 letter |>> charsToString) .>> spaces)
    |>> fun (num, type') -> { Type = type'; Quantity = num }

let pReaction : Parser<Reaction, unit> =
    sepBy1 pChemical (pstring ",") .>>. (pstring "=>" >>. pChemical)
    |>> fun (input, output) -> { Input = input; Output = output }

let getInput =
    File.ReadAllLines
    >> Array.toList
    >> List.map (run pReaction)
    >> List.choose (function Success (result, _, _) -> Some result | Failure _ -> None)

let ore = "ORE"
let fuel = "FUEL"

let asReactionsMap reactions =
    reactions
    |> List.map (fun reaction -> reaction.Output.Type, reaction)
    |> Map.ofList

let getQuantity chemicalType =
    Map.tryFind chemicalType >> Option.defaultValue 0L

let rec runReactionChain reactionsMap state target =
    if target.Type = ore then
        { state with UsedOre = state.UsedOre + target.Quantity }
    else
        let leftovers = state.LeftoverChemicals |> getQuantity target.Type
        if target.Quantity <= leftovers then
            let newLeftovers = state.LeftoverChemicals |> Map.add target.Type (leftovers - target.Quantity)
            { state with LeftoverChemicals = newLeftovers }
        else
            let reaction = reactionsMap |> Map.find target.Type

            let outputQuantity = reaction.Output.Quantity
            let quantity = target.Quantity - leftovers
            let multiplier = int64 <| ceil (float quantity / float outputQuantity)
            let newLeftovers = outputQuantity * multiplier - quantity

            let result =
                reaction.Input
                |> List.map (fun chemical -> { chemical with Quantity = chemical.Quantity * multiplier })
                |> List.fold (runReactionChain reactionsMap) state

            { result with LeftoverChemicals = result.LeftoverChemicals |> Map.add target.Type newLeftovers }

let rec useAllOre maxQuantity reactionMap currentFuelQuantity step =
    let result =
        runReactionChain
            reactionMap
            { UsedOre = 0L; LeftoverChemicals = Map.empty }
            { Type = fuel; Quantity = currentFuelQuantity }

    if result.UsedOre > maxQuantity && step = 1L
    then currentFuelQuantity - 1L
    elif result.UsedOre > maxQuantity
    then useAllOre maxQuantity reactionMap (currentFuelQuantity - step) (step / 2L)
    else useAllOre maxQuantity reactionMap (currentFuelQuantity + step) step

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let reactions = getInput file |> asReactionsMap

        let result =
            runReactionChain reactions { UsedOre = 0L; LeftoverChemicals = Map.empty } { Type = fuel; Quantity = 1L }

        printfn "%d" result.UsedOre

        printfn "%d" (useAllOre 1_000_000_000_000L reactions 0L 100_000L) 

        0
    | _ ->
        printfn "Usage: Aoc.Day14 file"
        1

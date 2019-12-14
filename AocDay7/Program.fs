module AocDay7.Program

open System.IO

let rec execute' inputs outputs index (nums : int []) = maybe {
    let! command = Command.ofInt nums.[index]
    let numParams = command.Operation |> Operation.numParameters

    if index < (nums |> Array.length) - numParams then
        let parameters =
            if numParams > 0 then
                [ index + 1 .. index + numParams - 1 ]
                |> List.map (fun i -> nums.[i])
                |> List.zip (command.ParameterModes |> withoutLast)
                |> List.map (fun (mode, num) -> match mode with Position -> nums.[num] | Immediate -> num)
            else
                []
            
        let location = nums.[index + numParams]

        let continue' inputs outputs = execute' inputs outputs (index + numParams + 1)

        let replaceAndContinue inputs outputs result =
            nums
            |> Array.mapi (fun i num -> if i = location then result else num)
            |> continue' inputs outputs

        let defaultReplaceAndContinue result = replaceAndContinue inputs outputs result

        let jump predicate = maybe {
            let! mode = command.ParameterModes |> List.rev |> List.tryHead

            let location =
                if parameters |> List.forall predicate then
                    let value = nums.[index + numParams]
                    match mode with
                    | Position -> nums.[value]
                    | Immediate -> value
                else
                    index + numParams + 1

            return! nums |> execute' inputs outputs location
        }

        match command.Operation with
        | Add ->
            return! parameters |> List.fold (+) 0 |> defaultReplaceAndContinue
        | Multiply ->
            return! parameters |> List.fold (*) 1 |> defaultReplaceAndContinue
        | Read ->
            return!
                match inputs with
                | input :: otherInputs -> input |> replaceAndContinue otherInputs outputs
                | [] -> Incomplete { Program = nums; Outputs = outputs; CurrentPosition = index } |> Some
        | Write ->
            let! mode = command.ParameterModes |> List.tryHead
            let result = match mode with Position -> nums.[location] | Immediate -> location
            return! nums |> continue' inputs (result :: outputs)
        | JumpIfTrue -> return! jump <| (<>) 0
        | JumpIfFalse -> return! jump <| (=) 0
        | LessThan ->
            let! a, b = parameters |> exactlyTwo
            return! (if a < b then 1 else 0) |> defaultReplaceAndContinue
        | Equals ->
            let! a, b = parameters |> exactlyTwo
            return! (if a = b then 1 else 0) |> defaultReplaceAndContinue
        | Halt ->
            return Complete { Program = nums; Outputs = outputs }
}

let execute inputs nums = execute' inputs [] 0 nums

let continue' inputs result = execute' inputs [] result.CurrentPosition result.Program

let plug previousResult currentResult = maybe {
    let! previousResult = previousResult
    return!
        match currentResult with
        | Complete result -> result |> Complete |> Some
        | Incomplete result -> continue' (previousResult |> ProgramResult.outputs) result
}

let amplify firstResult = List.scan plug (Some firstResult) >> sequenceOption >> Option.bind tryTail

let amplifyAll results = maybe {
    let firstResult = Complete { Program = Array.empty; Outputs = [0] }

    let! results = amplify firstResult results

    let rec continueAmplifying results = maybe {
        let! lastResult = results |> List.tryLast
        match lastResult with
        | Complete result ->
            return! result.Outputs |> List.tryLast
        | Incomplete _ ->
            let! newResults = amplify lastResult results
            return! continueAmplifying newResults
    }

    return! continueAmplifying results
}

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "The input file hasn't been specified"
    else
        let program =
            argv.[0]
            |> File.ReadAllText
            |> split ','
            |> Array.map stringToInt
            |> Array.choose id

        [ 5..9 ]
        |> perms
        |> Seq.map (List.map (fun setting -> execute [ setting ] program))
        |> Seq.map sequenceOption
        |> Seq.choose id
        |> Seq.map amplifyAll
        |> Seq.choose id
        |> Seq.max
        |> printfn "%i"
    0

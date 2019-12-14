module AocDay9.Program

open System.IO

let zero = int64 0
let one = int64 1

let rec execute' inputs outputs relativeBase index (nums : int64 []) = maybe {
    let! command = Command.ofInt (int nums.[index])
    let numParams = command.Operation |> Operation.numParameters

    if index < (nums |> Array.length) - numParams then
        let inputParameters =
            if numParams > 0 then
                [ index + 1 .. index + numParams - 1 ]
                |> List.map (fun i -> nums.[i])
                |> List.zip (command.ParameterModes |> withoutLast)
                |> List.map (fun (mode, num) ->
                    match mode with
                    | Position -> nums.[int num]
                    | Immediate -> num
                    | Relative -> nums.[int num + relativeBase])
            else
                []
            
        let lastParameter = nums.[index + numParams]

        let continue' inputs outputs relativeBase =
            execute' inputs outputs relativeBase (index + numParams + 1)

        let outputParameterMode =
            command.ParameterModes
            |> List.tryLast
            |> Option.defaultValue Position

        let replacePosition result i num =
            if i = int lastParameter then result else num

        let replaceRelative result i num =
            if i = int lastParameter + relativeBase then result else num

        let replace =
            match outputParameterMode with
            | Position | Immediate -> replacePosition
            | Relative -> replaceRelative

        let replaceAndContinue inputs outputs result =
            nums
            |> Array.mapi (replace result)
            |> continue' inputs outputs relativeBase

        let defaultReplaceAndContinue result =
            replaceAndContinue inputs outputs result

        let jump predicate =
            let location =
                if inputParameters |> List.forall predicate then
                    let value = nums.[index + numParams]
                    match outputParameterMode with
                    | Position -> nums.[int value]
                    | Immediate -> value
                    | Relative -> nums.[int value + relativeBase]
                    |> int
                else
                    index + numParams + 1

            nums |> execute' inputs outputs relativeBase location

        let outputParameter () =
            match outputParameterMode with
            | Position -> nums.[int lastParameter]
            | Immediate -> lastParameter
            | Relative -> nums.[int lastParameter + relativeBase]

        match command.Operation with
        | Add ->
            return! inputParameters |> List.fold (+) zero |> defaultReplaceAndContinue
        | Multiply ->
            return! inputParameters |> List.fold (*) one |> defaultReplaceAndContinue
        | Read ->
            return!
                match inputs with
                | input :: otherInputs -> input |> replaceAndContinue otherInputs outputs
                | [] -> Incomplete {
                    Program = nums
                    Outputs = outputs
                    CurrentPosition = index
                    CurrentRelativeBase = relativeBase } |> Some
        | Write ->
            let result = outputParameter ()
            return! nums |> continue' inputs (result :: outputs) relativeBase
        | JumpIfTrue -> return! jump <| (<>) zero
        | JumpIfFalse -> return! jump <| (=) zero
        | LessThan ->
            let! a, b = inputParameters |> exactlyTwo
            return! (if a < b then one else zero) |> defaultReplaceAndContinue
        | Equals ->
            let! a, b = inputParameters |> exactlyTwo
            return! (if a = b then one else zero) |> defaultReplaceAndContinue
        | AdjustRelativeBase ->
            let offset = outputParameter ()
            return! nums |> continue' inputs outputs (relativeBase + int offset)
        | Halt ->
            return Complete { Program = nums; Outputs = outputs }
}

let execute inputs nums = execute' inputs [] 0 0 nums

let continue' inputs result = execute' inputs [] result.CurrentPosition result.CurrentRelativeBase result.Program

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "The input file hasn't been specified"
    else
        let program =
            argv.[0]
            |> File.ReadAllText
            |> split ','
            |> Array.map stringToInt64
            |> Array.choose id

        Array.concat [ program; Array.replicate 100000 (int64 0) ]
        |> execute [int64 2]
        |> Option.map ProgramResult.outputs
        |> Option.defaultValue []
        |> List.map string
        |> List.rev
        |> List.fold (fun acc item -> if acc = "" then item else sprintf "%s, %s" acc item) ""
        |> printfn "%s"

    0

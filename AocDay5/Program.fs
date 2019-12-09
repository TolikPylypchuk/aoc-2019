module AocDay5.Program

open System.IO

let execute input output nums =
    let rec execute' index (nums : int []) = maybe {
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

            let continue' = execute' (index + numParams + 1)

            let replaceAndContinue result =
                nums
                |> Array.mapi (fun i num -> if i = location then result else num)
                |> continue'

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

                return! nums |> execute' location
            }

            match command.Operation with
            | Add ->
                return! parameters |> List.fold (+) 0 |> replaceAndContinue
            | Multiply ->
                return! parameters |> List.fold (*) 1 |> replaceAndContinue
            | Read ->
                return! input index nums |> replaceAndContinue
            | Write ->
                let! mode = command.ParameterModes |> List.tryHead
                let result = match mode with Position -> nums.[location] | Immediate -> location
                do output result
                return! nums |> continue'
            | JumpIfTrue -> return! jump <| (<>) 0
            | JumpIfFalse -> return! jump <| (=) 0
            | LessThan ->
                let! a, b = parameters |> exactlyTwo
                return! (if a < b then 1 else 0) |> replaceAndContinue
            | Equals ->
                let! a, b = parameters |> exactlyTwo
                return! (if a = b then 1 else 0) |> replaceAndContinue
            | Halt ->
                return nums
    }

    execute' 0 nums

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "The input file hasn't been specified"
    else
        argv.[0]
        |> File.ReadAllText
        |> split ','
        |> Array.map stringToInt
        |> Array.choose id
        |> execute (fun _ _ -> 5) (printf "%i ")
        |> ignore

        printfn ""
    0

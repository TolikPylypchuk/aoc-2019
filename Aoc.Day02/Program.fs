open System
open System.IO

type OpCode =
    | Add
    | Multiply
    | Halt

type IntCodeState = {
    Memory : int[]
    InstructionPointer : int
    IsHalted : bool
}

let toOpCode code =
    match code with
    | 1 -> Some Add
    | 2 -> Some Multiply
    | 99 -> Some Halt
    | _ -> None

let setCodeAt target newValue code =
    code |> Array.mapi (fun index item -> if index = target then newValue else item)

let execStep state =
    let perform op =
        let a = state.Memory.[state.Memory.[state.InstructionPointer + 1]]
        let b = state.Memory.[state.Memory.[state.InstructionPointer + 2]]
        let target = state.Memory.[state.InstructionPointer + 3]

        let newCode = state.Memory |> setCodeAt target (op a b)
        { state with Memory = newCode; InstructionPointer = state.InstructionPointer + 4 }

    if state.IsHalted then
        Some state
    else
        state.Memory.[state.InstructionPointer]
        |> toOpCode
        |> Option.map (function | Add -> perform (+) | Multiply -> perform (*) | Halt -> { state with IsHalted = true })

let rec exec state =
    match execStep state with
    | Some state when state.IsHalted -> Some state
    | Some state -> exec state
    | None -> None

let execAndGetResult memory =
    let initialState = { Memory = memory; InstructionPointer = 0; IsHalted = false }
    exec initialState |> Option.map (fun state -> state.Memory.[0])

let replaceMemory noun verb memory =
    memory |> Array.mapi (fun index item -> match index with 1 -> noun | 2 -> verb | _ -> item)

let strToInt (str : string) =
    let success, num = Int32.TryParse(str)
    if success then Some num else None

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> List.collect (fun nums -> nums.Split(',') |> List.ofArray)
    |> List.map strToInt
    |> List.choose id
    |> List.toArray
    
[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let input = getInput file

        match input |> replaceMemory 12 2 |> execAndGetResult with
        | Some result -> printfn "%d" result
        | None -> printfn "Error with the Intcode computer"

        [ 1 .. 99 ]
        |> List.allPairs [ 1 .. 99 ]
        |> List.map (fun (noun, verb) -> (noun, verb, input |> replaceMemory noun verb))
        |> List.map (fun (noun, verb, memory) ->
            execAndGetResult memory |> Option.map (fun result -> (noun, verb, result)))
        |> List.choose id
        |> List.map (fun (noun, verb, result) -> if result = 19690720 then Some (100 * noun + verb) else None)
        |> List.choose id
        |> List.iter (printfn "%d")

        0
    | _ ->
        printfn "Usage: Aoc.Day2 file"
        1

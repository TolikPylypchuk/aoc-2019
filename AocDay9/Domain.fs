namespace AocDay9

open System

type ParameterMode = Position | Immediate | Relative

type Operation =
    | Add
    | Multiply
    | Read
    | Write
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
    | AdjustRelativeBase
    | Halt

type Command = {
    Operation : Operation
    ParameterModes : ParameterMode list
}

type CompleteResult = {
    Program : int64 []
    Outputs : int64 list
}

type IncompleteResult = {
    Program : int64 []
    Outputs : int64 list
    CurrentPosition : int
    CurrentRelativeBase : int
}

type ProgramResult = Complete of CompleteResult | Incomplete of IncompleteResult

[<AutoOpen>]
module Util =
    
    let stringToInt64 (str : string) =
        let (success, value) = Int64.TryParse str
        if success then Some value else None
    
    let split (ch : char) (str : string) = str.Split(ch)

    let digitAt position num = num % (pown 10 position) / (pown 10 (position - 1))

    let withoutLast list = list |> List.rev |> List.tail |> List.rev

    let exactlyTwo list =
        match list with
        | [x; y] -> Some (x, y)
        | _ -> None

    let distrib e list =
        let rec distrib' pre post = seq {
            match post with
            | [] -> yield list @ [e]
            | head :: tail ->
                yield List.rev pre @ [e] @ post
                yield! distrib' (head :: pre) tail 
        }

        distrib' [] list
        
    let rec perms list =
        match list with
        | [] -> Seq.singleton []
        | head :: tail -> Seq.collect (distrib head) (perms tail)

    let tryTail = function _ :: tail -> Some tail | [] -> None

module ParameterMode =

    let ofInt param =
        match param with
        | 0 -> Some Position
        | 1 -> Some Immediate
        | 2 -> Some Relative
        | _ -> None

module Operation =

    let ofInt code =
        match code with
        | 1 -> Some Add
        | 2 -> Some Multiply
        | 3 -> Some Read
        | 4 -> Some Write
        | 5 -> Some JumpIfTrue
        | 6 -> Some JumpIfFalse
        | 7 -> Some LessThan
        | 8 -> Some Equals
        | 9 -> Some AdjustRelativeBase
        | 99 -> Some Halt
        | _ -> None

    let numParameters operation =
        match operation with
        | Add | Multiply | LessThan | Equals -> 3
        | JumpIfTrue | JumpIfFalse -> 2
        | Read | Write | AdjustRelativeBase -> 1
        | Halt -> 0

module Command =

    let ofInt command = maybe {
        let! operation = command % 100 |> Operation.ofInt
        let numParameters = operation |> Operation.numParameters
        
        let! parameterModes =
            [ 0 .. numParameters - 1 ]
            |> List.map (fun i -> command |> digitAt (i + 3))
            |> List.map ParameterMode.ofInt
            |> sequenceOption
            
        return {
            Operation = operation
            ParameterModes = parameterModes
        }
    }

module ProgramResult =

    let outputs result =
        match result with
        | Incomplete result -> result.Outputs
        | Complete result -> result.Outputs
    
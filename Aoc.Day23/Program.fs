open System
open System.Numerics
open System.IO
open System.Threading

open FSharpPlus
open FSharpPlus.Data

type OpCode =
    | Add
    | Multiply
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | Less
    | Equals
    | OffsetRelativeBase
    | Halt

type ParameterMode =
    | Position
    | Immediate
    | Relative

type Instruction = {
    Code : OpCode
    ParameterModes : ParameterMode list
}

type IntCodeMemory = IntCodeMemory of Map<bigint, bigint>

type IntCodeState = {
    InstructionPointer : bigint
    RelativeBase : bigint
    Input : bigint list
    Output : bigint list
    IsPaused : bool
    IsHalted : bool
}

type IntCodeMessage = {
    Address : bigint
    X : bigint
    Y : bigint
}

type Message =
    | InitNetwork of Map<bigint, MailboxProcessor<Message>>
    | Data of IntCodeMessage

type NatMessage =
    | SetReceiver of MailboxProcessor<Message>
    | ActorIdle of bigint
    | ActorNotIdle of bigint
    | NatData of IntCodeMessage

type NatState = {
    IdleActors : Set<bigint>
    Receiver : MailboxProcessor<Message> option
    LastSentMessage : IntCodeMessage option
    MessageToSend : IntCodeMessage option
    MessageSent : bool
}

let opCode code =
    match int code with
    | 1 -> Some Add
    | 2 -> Some Multiply
    | 3 -> Some Input
    | 4 -> Some Output
    | 5 -> Some JumpIfTrue
    | 6 -> Some JumpIfFalse
    | 7 -> Some Less
    | 8 -> Some Equals
    | 9 -> Some OffsetRelativeBase
    | 99 -> Some Halt
    | _ -> None

let flippedDigits = List.unfold (fun num -> if num = (bigint 0) then None else Some (num % 10I, num / 10I))

let parameterMode mode =
    match int mode with
    | 0 -> Some Position
    | 1 -> Some Immediate
    | 2 -> Some Relative
    | _ -> None

let numParameters =
    function
    | Add -> 3
    | Multiply -> 3
    | Input -> 1
    | Output -> 1
    | JumpIfTrue -> 2
    | JumpIfFalse -> 2
    | Less -> 3
    | Equals -> 3
    | OffsetRelativeBase -> 1
    | Halt -> 0

let toInstruction instruction = monad {
    let! code = instruction % 100I |> opCode
    let! modes =
        instruction / 100I
        |> flippedDigits
        |> List.map parameterMode
        |> sequence

    let expected = code |> numParameters
    let actual = modes |> List.length

    return { Code = code; ParameterModes = modes @ List.replicate (expected - actual) Position }
}

let get index (IntCodeMemory memory) =
    memory |> Map.tryFind index |> Option.defaultValue zero

let set relativeBase mode target newValue (IntCodeMemory memory) =
    let actualTarget =
        match mode with
        | Relative -> target + relativeBase
        | _ -> target

    memory |> Map.add actualTarget newValue |> IntCodeMemory

let getParameter relativeBase position mode memory =
    match mode with
    | Position -> memory |> get (memory |> get position)
    | Immediate -> memory |> get position
    | Relative -> memory |> get ((memory |> get position) + relativeBase)

let execStep memory = monad {
    let! state = State.get

    let getParameter = getParameter state.RelativeBase
    let set = set state.RelativeBase

    let performBinary mode op (mode1, mode2) =
        let a = memory |> getParameter (state.InstructionPointer + 1I) mode1
        let b = memory |> getParameter (state.InstructionPointer + 2I) mode2
        let target = memory |> get (state.InstructionPointer + 3I)

        memory |> set mode target (op a b)

    let jump condition (mode1, mode2) =
        let test = memory |> getParameter (state.InstructionPointer + 1I) mode1
        let target = memory |> getParameter (state.InstructionPointer + 2I) mode2

        if test |> condition
        then target
        else state.InstructionPointer + 3I

    let setConditionalValue mode op (mode1, mode2) =
        let param1 = memory |> getParameter (state.InstructionPointer + 1I) mode1
        let param2 = memory |> getParameter (state.InstructionPointer + 2I) mode2
        let target = memory |> get (state.InstructionPointer + 3I)

        memory |> set mode target (if op param1 param2 then one else zero)

    if state.IsHalted || state.IsPaused then
        return memory |> Some
    else
        let instruction = memory |> get state.InstructionPointer |> toInstruction
        match instruction with

        | Some { Code = Add; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4I }
            return performBinary mode (+) (mode1, mode2) |> Some

        | Some { Code = Multiply; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4I }
            return performBinary mode (*) (mode1, mode2) |> Some

        | Some { Code = Input; ParameterModes = [ mode ] } ->
            match state.Input with
            | input :: inputs ->
                let param = memory |> get (state.InstructionPointer + one)
                do! State.put { state with InstructionPointer = state.InstructionPointer + 2I; Input = inputs }
                return memory |> set mode param input |> Some
            | [] ->
                do! State.put { state with IsPaused = true }
                return memory |> Some

        | Some { Code = Output; ParameterModes = [ mode ] } ->
            let output = memory |> getParameter (state.InstructionPointer + one) mode
            do! State.put {
                state with InstructionPointer = state.InstructionPointer + 2I; Output = state.Output @ [ output ]
            }
            return memory |> Some

        | Some { Code = JumpIfTrue; ParameterModes = [ mode1; mode2 ] } ->
            do! State.put { state with InstructionPointer = jump ((<>) zero) (mode1, mode2) }
            return memory |> Some

        | Some { Code = JumpIfFalse; ParameterModes = [ mode1; mode2 ] } ->
            do! State.put { state with InstructionPointer = jump ((=) zero) (mode1, mode2) }
            return memory |> Some

        | Some { Code = Less; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4I }
            return setConditionalValue mode (<) (mode1, mode2) |> Some

        | Some { Code = Equals; ParameterModes = [ mode1; mode2; mode ] } ->
            do! State.put { state with InstructionPointer = state.InstructionPointer + 4I }
            return setConditionalValue mode (=) (mode1, mode2) |> Some

        | Some { Code = OffsetRelativeBase; ParameterModes = [ mode ] } ->
            let newPointer = state.InstructionPointer + 2I
            let newRelativeBase = state.RelativeBase + (memory |> getParameter (state.InstructionPointer + one) mode)
            do! State.put { state with InstructionPointer = newPointer; RelativeBase = newRelativeBase }
            return memory |> Some

        | Some { Code = Halt; ParameterModes = [] } ->
            do! State.put { state with IsHalted = true }
            return memory |> Some

        | _ ->
            return None
}

let rec exec memory = monad {
    match! execStep memory with
    | Some memory ->
        let! state = State.get
        if not (state.IsHalted || state.IsPaused)
        then return! exec memory
        else return Some memory
    | None ->
        return None
}

let strToBigInt (str : string) =
    let success, num = BigInteger.TryParse(str)
    if success then Some num else None

let getInput file =
    file
    |> File.ReadAllLines
    |> List.ofArray
    |> List.collect (fun nums -> nums.Split(',') |> List.ofArray)
    |> List.map strToBigInt
    |> List.choose id
    |> List.mapi (fun index item -> (bigint index, item))
    |> Map.ofList
    |> IntCodeMemory

let initialState address = {
    InstructionPointer = 0I
    RelativeBase = 0I
    Input = [ address ]
    Output = []
    IsPaused = false
    IsHalted = false
}

let createNat numActors receiverAddress = MailboxProcessor.Start <| fun inbox ->
    let rec loop state = async {
        match! inbox.TryReceive(10000) with
        | Some (SetReceiver receiver) ->
            return! loop { state with Receiver = Some receiver }
        | Some (ActorIdle address) ->
            let idleActors = state.IdleActors |> Set.add address

            if numActors = (idleActors |> Set.count) then
                match state.Receiver, state.MessageToSend, state.MessageSent with
                | Some receiver, Some message, false ->
                    printfn "NAT: Network is idle. Sendning %A, %A to %A" message.X message.Y receiverAddress
                    receiver.Post(Data { message with Address = receiverAddress })

                    match state.LastSentMessage with
                    | Some lastMessage when message.Y = lastMessage.Y ->
                        printfn "NAT: The Y of the repeating message is %A" message.Y
                    | _ ->
                        ()

                    return! loop {
                        state with IdleActors = idleActors; LastSentMessage = state.MessageToSend; MessageSent = true
                    }
                | _ ->
                    return! loop { state with IdleActors = idleActors }
            else
                return! loop { state with IdleActors = idleActors }
        | Some (ActorNotIdle address) ->
            return! loop { state with IdleActors = state.IdleActors |> Set.remove address; MessageSent = false }
        | Some (NatData message) ->
            printfn "NAT: Received a data message: %A, %A" message.X message.Y
            return! loop { state with MessageToSend = Some message }
        | None ->
            return! loop state
    }

    loop { IdleActors = Set.empty; Receiver = None; LastSentMessage = None; MessageToSend = None; MessageSent = false }

let createActor (nat : MailboxProcessor<NatMessage>) memory address = MailboxProcessor.Start <| fun inbox ->
    let rec exec network memory state = async {
        let! message = inbox.TryReceive(1)

        match message with
        | Some (InitNetwork network) ->
            return! exec (Some network) memory state
        | _ ->
            let newInput =
                match message with
                | Some (Data message) when message.Address = address ->
                    [ message.X; message.Y ]
                | _ ->
                    []

            let isNotIdle = (state.Input |> List.length) >= 3

            let memory, state = State.run (execStep memory) { state with Input = state.Input @ newInput }

            match memory with
            | Some memory when not state.IsHalted ->
                if state.IsPaused then
                    nat.Post(ActorIdle address)
                    return! exec network memory { state with IsPaused = false; Input = [ -1I ] }
                else
                    if isNotIdle
                    then nat.Post(ActorNotIdle address)

                    let newOutput =
                        match state.Output, network with
                        | receiverAddress :: x :: y :: otherOutput, Some network ->
                            let message = { Address = receiverAddress; X = x; Y = y }

                            if receiverAddress = 255I then
                                nat.Post(NatData message)
                            else
                                network
                                |> Map.tryFind message.Address
                                |> Option.iter (fun actor -> actor.Post (Data message))

                            otherOutput
                        | _ ->
                            state.Output

                    return! exec network memory { state with IsPaused = false; Output = newOutput }
            | _ ->
                return ()
    }

    exec None memory (initialState address)

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let memory = file |> getInput

        let nat = createNat 50 0I

        let actors =
            [ 0I..49I ]
            |> List.map (createActor nat memory)

        nat.Post(actors |> List.head |> SetReceiver)

        let network =
            actors
            |> List.indexed
            |> List.map (fun (index, actor) -> bigint index, actor)
            |> Map.ofList

        actors |> List.iter (fun actor -> actor.Post (InitNetwork network))

        Thread.Sleep(3600_000)

        0
    | _ ->
        printfn "Usage: Aoc.Day23 file"
        1

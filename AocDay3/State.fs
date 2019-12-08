// Implementation taken from here: https://gist.github.com/jwosty/5338fce8a6691bbd9f6f

namespace AocDay3

type State<'s, 'a> = State of ('s -> ('a * 's))

module State =

    let inline run state x = let (State f) = x in f state
    
    let inline eval state x = run state x |> fst

    let inline exec state x = run state x |> snd

    let retn x = State (fun s -> x, s)

    let get = State (fun s -> s, s)

    let put newState = State (fun _ -> (), newState)

    let map f s = State (fun (state: 's) -> let x, state = run state s in f x, state)

    let bind f s = State (fun state -> let (result: 'a), state = run state s in run state (f result))

type StateBuilder() =

    member _.Zero() = State.retn ()

    member _.Return x = State.retn x

    member inline _.ReturnFrom (x: State<'s, 'a>) = x

    member _.Bind(x, f) : State<'s, 'b> = State.bind f x

    member _.Combine(x1: State<'s, 'a>, x2: State<'s, 'b>) =
        State (fun state -> let _, state = State.run state x1 in State.run state x2)

    member _.Delay f : State<'s, 'a> = f ()

    member this.For(seq, (f: 'a -> State<'s, 'b>)) =
        seq
        |> Seq.map f
        |> Seq.reduceBack (fun x1 x2 -> this.Combine (x1, x2))

    member this.While(f, x) =
        if f () then this.Combine (x, this.While (f, x))
        else this.Zero ()

[<AutoOpen>]
module StateDefinition =

    let state = new StateBuilder()

namespace AocDay3

type MaybeBuilder() =

    member _.Return x = Some x

    member _.ReturnFrom x = x

    member _.Bind(x, f) = x |> Option.bind f

[<AutoOpen>]
module MaybeDefinition =

    let maybe = MaybeBuilder()

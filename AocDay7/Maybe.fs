namespace AocDay7

type MaybeBuilder() =

    member _.Zero() = None

    member _.Return x = Some x

    member _.ReturnFrom x = x

    member _.Bind(x, f) = x |> Option.bind f

[<AutoOpen>]
module MaybeDefinition =

    let maybe = MaybeBuilder()

    let traverseOption f list =
        let folder head tail = maybe {
            let! h = f head
            let! t = tail
            return h :: t
        }

        Some [] |> List.foldBack folder list

    let sequenceOption list = traverseOption id list

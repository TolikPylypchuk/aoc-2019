namespace AocDay6

type Orbit = {
    BiggerObject : string
    SmallerObject : string
}

type 'a tree = Leaf of 'a | Node of 'a * 'a tree list

[<AutoOpen>]
module Util =

    let split (ch : char) (str : string) = str.Split(ch) |> Array.toList

    let exactlyTwo = function [x; y] -> Some (x, y) | _ -> None

module Orbit =

    let ofPair (x, y) = { BiggerObject = x; SmallerObject = y }

    let biggerObject orbit = orbit.BiggerObject
    let smallerObject orbit = orbit.SmallerObject

module Tree =

    let rec map f = function Leaf x -> Leaf (f x) | Node (x, subtree) -> Node (f x, subtree |> List.map (map f))

    let rec sum (tree : int tree) =
        match tree with
        | Leaf x -> x
        | Node (x, subtree) -> x + (subtree |> List.sumBy sum)

module OrbitTree =

    let ofOrbits orbits = maybe {
        let groupedOrbits = orbits |> List.groupBy Orbit.biggerObject |> Map.ofList

        let rec createTree currentObject =
            let smallerObjects =
                groupedOrbits
                |> Map.tryFind currentObject
                |> Option.defaultValue []
                |> List.map Orbit.smallerObject
                |> List.map createTree

            if smallerObjects |> List.isEmpty
            then Leaf currentObject
            else Node (currentObject, smallerObjects)

        let! rootObject =
            orbits
            |> List.map Orbit.biggerObject
            |> List.filter (fun orbit -> orbits |> List.forall (fun o -> o |> Orbit.smallerObject <> orbit))
            |> List.tryExactlyOne

        return createTree rootObject
    }

    let numOrbits orbitTree =
        let rec numOrbits' =
            function
            | Leaf _ ->
                Leaf 0
            | Node (_, subtree) ->
                let newSubtree = subtree |> List.map numOrbits'
                let sum = (newSubtree |> List.sumBy (function | Leaf _ -> 0 | Node (x, _) -> x)) + (subtree |> List.length)
                Node (sum, newSubtree)

        orbitTree |> numOrbits' |> Tree.sum

    let numTransfers from to' tree =
        let rec contains tag tree =
            match tree with
            | Leaf x -> x = tag
            | Node (x, subtree) -> x = tag || (subtree |> List.filter (contains tag) |> List.isEmpty |> not)

        let rec path tag tree =
            match tree with
            | Leaf x -> [x]
            | Node (x, subtree) ->
                let subpath =
                    subtree
                    |> List.filter (contains tag)
                    |> List.tryExactlyOne
                    |> Option.map (path tag)
                    |> Option.defaultValue []
                x :: subpath

        let rec zipAndPad xs ys =
            match (xs, ys) with
            | [], [] -> []
            | [], ys -> zipAndPad [""] ys
            | xs, [] -> zipAndPad xs [""]
            | x :: xs, y :: ys -> (x, y) :: zipAndPad xs ys

        zipAndPad (tree |> path from) (tree |> path to')
        |> List.filter (fun (x, y) -> x <> y)
        |> List.sumBy (fun (x, y) -> if x = "" || y = "" then 1 else 2)

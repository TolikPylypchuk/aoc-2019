open System.IO

type Tile =
    | Bug
    | Empty

let maxX = 5
let maxY = 5

let midX = 3
let midY = 3

let emptyLayout = array2D (List.replicate (maxY + 2) (List.replicate (maxX + 2) Empty))

let parseTile = function '#' -> Bug | _ -> Empty

let getInput file =
    let input =
        file
        |> File.ReadAllLines
        |> Array.map (fun row -> $".{row}.")
        |> Array.toList

    let border = String.replicate 7 "."
    let completeInput = border :: (input @ [ border ])

    completeInput
    |> List.map (Seq.map parseTile)
    |> array2D

let countAdjacentBugs (x, y) (tiles : Tile[,]) =
    [
        tiles.[y - 1, x]
        tiles.[y + 1, x]
        tiles.[y, x - 1]
        tiles.[y, x + 1]
    ]
    |> List.filter ((=) Bug)
    |> List.length

let nextLayout tiles =
    tiles
    |> Array2D.mapi (fun y x tile ->
        if x = 0 || x = maxX + 1 || y = 0 || y = maxY + 1 then
            tile
        else
            let adjacentBugs = tiles |> countAdjacentBugs (x, y)
            if tile = Bug then
                if adjacentBugs = 1 then Bug else Empty
            else
                if adjacentBugs = 1 || adjacentBugs = 2 then Bug else Empty)

let areLayoutsEqual tiles1 (tiles2 : Tile[,]) =
    tiles1
    |> Array2D.mapi (fun y x tile -> y, x, tile)
    |> Seq.cast<int * int * Tile>
    |> Seq.forall (fun (y, x, tile) -> tiles2.[y, x] = tile)

let rec generateUntilRepeats previousLayouts tiles =
    let newTiles = tiles |> nextLayout

    let previousLayouts = tiles :: previousLayouts

    let isSame =
        previousLayouts
        |> List.exists (areLayoutsEqual newTiles)

    if isSame
    then newTiles
    else newTiles |> generateUntilRepeats previousLayouts

let countAdjacentBugsRecursive level (x, y) (allTiles : Map<int, Tile[,]>) =
    match allTiles |> Map.tryFind level with
    | Some tiles ->
        [
            if y = 1 then
                match allTiles |> Map.tryFind (level - 1) with
                | Some upperTiles ->
                    yield upperTiles.[midY - 1, midX]
                | None ->
                    yield Empty
            elif y = midY + 1 && x = midX then
                match allTiles |> Map.tryFind (level + 1) with
                | Some lowerTiles ->
                    yield! [ 1 .. maxX ] |> List.map (fun x -> lowerTiles.[maxY, x])
                | None ->
                    yield Empty
            else
                yield tiles.[y - 1, x]

            if y = maxY then
                match allTiles |> Map.tryFind (level - 1) with
                | Some upperTiles ->
                    yield upperTiles.[midY + 1, midX]
                | None ->
                    yield Empty
            elif y = midY - 1 && x = midX then
                match allTiles |> Map.tryFind (level + 1) with
                | Some lowerTiles ->
                    yield! [ 1 .. maxX ] |> List.map (fun x -> lowerTiles.[1, x])
                | None ->
                    yield Empty
            else
                yield tiles.[y + 1, x]

            if x = 1 then
                match allTiles |> Map.tryFind (level - 1) with
                | Some upperTiles ->
                    yield upperTiles.[midY, midX - 1]
                | None ->
                    yield Empty
            elif x = midX + 1 && y = midY then
                match allTiles |> Map.tryFind (level + 1) with
                | Some lowerTiles ->
                    yield! [ 1 .. maxY ] |> List.map (fun y -> lowerTiles.[y, maxX])
                | None ->
                    yield Empty
            else
                yield tiles.[y, x - 1]

            if x = maxX then
                match allTiles |> Map.tryFind (level - 1) with
                | Some upperTiles ->
                    yield upperTiles.[midY, midX + 1]
                | None ->
                    yield Empty
            elif x = midX - 1 && y = midY then
                match allTiles |> Map.tryFind (level + 1) with
                | Some lowerTiles ->
                    yield! [ 1 .. maxY ] |> List.map (fun y -> lowerTiles.[y, 1])
                | None ->
                    yield Empty
            else
                yield tiles.[y, x + 1]
        ]
        |> List.filter ((=) Bug)
        |> List.length
    | None ->
        0

let nextLayoutRecursive allTiles =
    allTiles
    |> Map.map (fun level tiles ->
        tiles
        |> Array2D.mapi (fun y x tile ->
            if x = 0 || x = maxX + 1 || y = 0 || y = maxY + 1 || x = midX && y = midY then
                tile
            else
                let adjacentBugs = allTiles |> countAdjacentBugsRecursive level (x, y)
                if tile = Bug then
                    if adjacentBugs = 1 then Bug else Empty
                else
                    if adjacentBugs = 1 || adjacentBugs = 2 then Bug else Empty))

let generateRecursive maxIteration tiles =
    let rec gen iteration tiles =
        if iteration = maxIteration then
            tiles
        else
            let newLevel = iteration + 1
            let tiles = tiles |> Map.add newLevel emptyLayout |> Map.add -newLevel emptyLayout
            gen (iteration + 1) (tiles |> nextLayoutRecursive)

    gen 0 ([ 0, tiles ] |> Map.ofList)

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let tiles = file |> getInput
        let newTiles = tiles |> generateUntilRepeats []

        newTiles
        |> Array2D.mapi (fun y x tile ->
            if x = 0 || x = maxX + 1 || y = 0 || y = maxY + 1 || tile = Empty
            then 0
            else pown 2 ((y - 1) * (maxX - 1) + x - 1))
        |> Seq.cast<int>
        |> Seq.sum
        |> printfn "%d"

        tiles
        |> generateRecursive 200
        |> Map.values
        |> Seq.map (fun tiles -> tiles |> Array2D.map (function Bug -> 1 | Empty -> 0) |> Seq.cast<int> |> Seq.sum)
        |> Seq.sum
        |> printfn "%d"

        0
    | _ ->
        printfn "Usage: Aoc.Day24 file"
        1

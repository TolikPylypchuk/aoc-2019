open System
open System.IO

type TileType =
    | Empty
    | Start
    | Key of char
    | Door of char

type Tile = {
    X : int
    Y : int
    Type : TileType
}

type Maze = Maze of Map<Tile, Tile list>

type ShortestPathState = {
    CurrentTiles : Tile list
    CurrentPath : Tile list
    ShortestPaths : Map<Tile, Map<Tile, Tile list>>
    ShortestFullPath : Tile list option
    Dependencies : Map<Tile, char list>
    TotalKeys : int
    CollectedKeys : char list
    Cache : Map<Tile list, (Set<char> * int) list>
}

let (|Lower|_|) = Some >> Option.filter Char.IsLower
let (|Upper|_|) = Some >> Option.filter Char.IsUpper

let parseTileType =
    function
    | '.' -> Some Empty
    | '@' -> Some Start
    | Lower ch -> Some <| Key ch
    | Upper ch -> Some <| Door ch
    | _ -> None

let toChar tile =
    match tile.Type with
    | Empty -> '.'
    | Start -> '@'
    | Key ch -> ch
    | Door ch -> ch

let linesToArray lines =
    lines
    |> Array.mapi (fun y -> Seq.mapi (fun x -> parseTileType >> Option.map (fun t -> { X = x; Y = y; Type = t })))
    |> array2D

let findAdjacentTiles (tiles : Tile option[,]) tile =
    [ tiles.[tile.Y - 1, tile.X]; tiles.[tile.Y + 1, tile.X]; tiles.[tile.Y, tile.X - 1]; tiles.[tile.Y, tile.X + 1] ]
    |> List.choose id

let arrayToMaze tiles =
    tiles
    |> Seq.cast<Tile option>
    |> Seq.choose id
    |> Seq.map (fun tile -> tile, tile |> findAdjacentTiles tiles)
    |> Map.ofSeq
    |> Maze

let getInput file =
    file
    |> File.ReadAllLines
    |> linesToArray
    |> arrayToMaze

let findTiles type' (Maze maze) =
    maze
    |> Map.keys
    |> Seq.filter (fun tile -> tile.Type = type')
    |> Seq.toList

let allKeys (Maze maze) =
    maze
    |> Map.keys
    |> Seq.filter (fun tile -> match tile.Type with Key _ -> true | _ -> false)
    |> Seq.toList

let distance tile1 tile2 =
    ((abs tile1.X) - (abs tile2.X)) + ((abs tile1.Y) - (abs tile2.Y))

let findShortestPath (Maze maze) (start, goal) =
    let rec reconstructPath cameFrom current =
        seq {
            yield current
            match Map.tryFind current cameFrom with
            | None -> ()
            | Some next -> yield! reconstructPath cameFrom next
        }

    let rec crawler closedSet (openSet, gScores, fScores, cameFrom) =
        match openSet |> List.sortBy (fun tile -> fScores |> Map.find tile) with
        | current :: _ when current = goal -> Some <| reconstructPath cameFrom current
        | current :: rest ->
            let gScore = gScores |> Map.find current
            let next =
                maze
                |> Map.find current
                |> List.filter (fun tile -> closedSet |> Set.contains tile |> not)
                |> List.fold (fun (openSet, gScores, fScores, cameFrom) neighbour ->
                    let tentativeGScore = gScore + 1
                    if (openSet |> List.contains neighbour) && tentativeGScore >= (gScores |> Map.find neighbour) then
                        (openSet, gScores, fScores, cameFrom)
                    else
                        let newOpenSet = if openSet |> List.contains neighbour then openSet else neighbour :: openSet
                        let newGScores = gScores |> Map.add neighbour tentativeGScore
                        let newFScores = fScores |> Map.add neighbour (tentativeGScore + distance neighbour goal)
                        let newCameFrom = cameFrom |> Map.add neighbour current
                        newOpenSet, newGScores, newFScores, newCameFrom)
                    (rest, gScores, fScores, cameFrom)
            crawler (Set.add current closedSet) next
        | _ -> None

    let gScores = Map.ofList [ start, 0 ]
    let fScores = Map.ofList [ start, distance start goal ]

    crawler Set.empty ([ start ], gScores, fScores, Map.empty)
    |> Option.map Seq.toList
    |> Option.map List.rev
    |> Option.map List.tail
    |> Option.defaultValue []

let makeSymmetric shortestPaths =
    shortestPaths
    |> Map.toList
    |> List.collect (fun (start, tiles) ->
        tiles
        |> Map.toList
        |> List.collect (fun (finish, path) ->
            [ start, (finish, path); finish, (start, (start :: path) |> List.rev |> List.tail) ] ))
    |> List.groupBy fst
    |> List.map (fun (tile, tiles) -> tile, tiles |> List.map snd |> Map.ofList)
    |> Map.ofList

let mergeMaps map1 map2 =
    map1 |> Map.fold (fun acc key value -> acc |> Map.add key value) map2

let calculateShortestPaths maze =
    let keys = maze |> allKeys

    let shortestPaths =
        keys
        |> List.allPairs keys
        |> List.filter (fun (t1, t2) -> (t1 |> toChar) < (t2 |> toChar))
        |> List.map (fun (t1, t2) -> t1, (t2, (t1, t2) |> findShortestPath maze))
        |> List.groupBy fst
        |> List.map (fun (tile, tiles) ->
            tile, tiles |> List.map snd |> List.filter (snd >> List.isEmpty >> not) |> Map.ofList)
        |> Map.ofList
        |> makeSymmetric

    let startTiles = maze |> findTiles Start

    let pathsFromStartTiles =
        keys
        |> List.allPairs startTiles
        |> List.map (fun (start, tile) -> start, (tile, (start, tile) |> findShortestPath maze))
        |> List.groupBy fst
        |> List.map (fun (start, tiles) ->
            start, tiles |> List.map snd |> List.filter (snd >> List.isEmpty >> not) |> Map.ofList)
        |> Map.ofList

    mergeMaps shortestPaths pathsFromStartTiles

let calculateDependencies shortestPaths tile =
    shortestPaths
    |> Map.find tile
    |> Map.map (fun _ -> List.choose (fun t -> match t.Type with Door ch -> Some ch | _ -> None))

let removeDependencies tile dependencies =
    match tile.Type with
    | Key ch ->
        let door = Char.ToUpper(ch)
        dependencies |> Map.map (fun _ -> List.filter ((<>) door))
    | _ ->
        dependencies

let removeCollectedKey shortestPaths tile =
    shortestPaths
    |> Map.map (fun _ -> Map.map (fun _ -> List.map (fun t -> if t = tile then { t with Type = Empty } else t)))

let removeCollectedKeys tiles shortestPaths =
    tiles
    |> List.fold removeCollectedKey shortestPaths

let alreadyHasBetterPath state tile =
    let currentPathLength = state.CurrentPath |> List.length
    let collectedKeys = Set.ofList state.CollectedKeys

    state.Cache
    |> Map.tryFind tile
    |> Option.map (List.exists <| fun (keys, len) -> len <= currentPathLength && Set.isSubset collectedKeys keys)
    |> Option.defaultValue false

let findPathsToConsider state paths =
    paths
    |> Map.toList
    |> List.filter (fun (target, _) -> state.CollectedKeys |> List.contains (target |> toChar) |> not)
    |> List.filter (fun (target, _) ->
        state.Dependencies |> Map.tryFind target |> Option.defaultValue [] |> List.isEmpty)
    |> List.filter (fun (_, path) ->
        path
        |> List.rev
        |> List.tail
        |> List.exists (fun t -> match t.Type with Key _ -> true | _ -> false)
        |> not)
    |> List.map snd
    |> List.sortBy List.length

let updateCache tiles state =
    let currentLength = state.CurrentPath |> List.length

    let newCacheItem =
        state.Cache
        |> Map.tryFind tiles
        |> Option.map (fun paths -> (Set.ofList state.CollectedKeys, currentLength) :: paths)
        |> Option.defaultWith (fun () -> (Set.ofList state.CollectedKeys, currentLength) :: [])

    state.Cache |> Map.add tiles newCacheItem

let findSubstitutedCurrentTile path state =
    let tile = path |> List.head
    state.CurrentTiles
    |> List.pick (fun currentTile ->
        if currentTile.Y = tile.Y && abs (currentTile.X - tile.X) = 1 ||
           currentTile.X = tile.X && abs (currentTile.Y - tile.Y) = 1
        then path |> List.tryLast |> Option.map (fun tile -> currentTile, tile)
        else None)

let rec findFullShortestPath state =
    if (state.CurrentPath |> List.length) >=
        (state.ShortestFullPath |> Option.map List.length |> Option.defaultValue Int32.MaxValue) then
        state
    elif (state.CollectedKeys |> List.length) = state.TotalKeys then
        { state with
            ShortestFullPath =
                state.ShortestFullPath
                |> Option.map (fun path -> [ path; state.CurrentPath ] |> List.minBy List.length)
                |> Option.defaultValue state.CurrentPath
                |> Some
        }
    elif state.CurrentTiles |> alreadyHasBetterPath state then
        state
    else
        let pathsToConsider =
            state.CurrentTiles
            |> List.choose (fun tile -> state.ShortestPaths |> Map.tryFind tile)
            |> List.fold mergeMaps Map.empty
            |> findPathsToConsider state

        let state = {
            state with
                ShortestPaths = state.ShortestPaths |> removeCollectedKeys state.CurrentTiles
                Cache = state |> updateCache state.CurrentTiles
        }

        pathsToConsider
        |> List.fold
            (fun potentialShortestPathState path ->
                let oldTile, newTile = state |> findSubstitutedCurrentTile path
                let newCurrentTiles =
                    state.CurrentTiles |> List.map (fun tile -> if tile = oldTile then newTile else tile)

                let newState = {
                    state with
                        CurrentTiles = newCurrentTiles
                        ShortestPaths = state.ShortestPaths |> removeCollectedKeys newCurrentTiles
                        Dependencies = state.Dependencies |> removeDependencies newTile
                        CollectedKeys = (newTile |> toChar) :: state.CollectedKeys
                        ShortestFullPath = potentialShortestPathState.ShortestFullPath
                        CurrentPath = state.CurrentPath @ path
                        Cache = potentialShortestPathState.Cache
                }

                findFullShortestPath newState)
            state

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let maze = file |> getInput
        let shortestPaths = maze |> calculateShortestPaths
        let startTiles = maze |> findTiles Start
        let dependencies =
            startTiles
            |> List.map (calculateDependencies shortestPaths)
            |> List.fold (mergeMaps) Map.empty

        let state = {
            CurrentTiles = startTiles
            CurrentPath = []
            ShortestPaths = shortestPaths
            ShortestFullPath = None
            Dependencies = dependencies
            TotalKeys = maze |> allKeys |> List.length
            CollectedKeys = []
            Cache = Map.empty
        }

        let targetState = state |> findFullShortestPath

        targetState.ShortestFullPath
        |> Option.map List.length
        |> Option.iter (printfn "%d")

        0
    | _ ->
        printfn "Usage: Aoc.Day18 file"
        1

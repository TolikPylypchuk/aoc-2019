open System
open System.IO

type TileType =
    | Passage
    | Wall
    | PortalPart of char

type Tile = {
    X : int
    Y : int
    Type : TileType
}

type PortalType = Inner | Outer

type Portal = {
    Label : string
    PortalType : PortalType
}

type Maze = Maze of Map<Portal, (Portal * int) list>

type PathState = {
    SourcePortal : Portal
    CurrentTile : Tile
    CurrentPathLength : int
    MarkedTiles : Set<Tile>
    MaxX : int
    MaxY : int
}

type ShortestPathState = {
    CurrentPortal : Portal
    CurrentLevel : int
    VisitedPortals : Set<Portal * int>
    ShortestPaths : Map<Portal * int, int>
}

let (|Letter|_|) = Some >> Option.filter Char.IsLetter

let parseTileType =
    function
    | '.' -> Some Passage
    | '@' -> Some Wall
    | Letter ch -> Some <| PortalPart ch
    | _ -> None

let initialPathState portal tile maxX maxY = {
    SourcePortal = portal
    CurrentTile = tile
    CurrentPathLength = 0
    MarkedTiles = Set.singleton tile
    MaxX = maxX
    MaxY = maxY
}

let concatPortal (actualMaxX, actualMaxY) (tile1, tile2) (label1, label2) =
    let label1 = string label1
    let label2 = string label2

    let minX = min tile1.X tile2.X
    let maxX = max tile1.X tile2.X
    let minY = min tile1.Y tile2.Y
    let maxY = max tile1.Y tile2.Y

    {
        Label =
            if minX = tile1.X && minY = tile1.Y
            then label1 + label2
            else label2 + label1
        PortalType =
            if minX = 0 || minY = 0 || maxX = actualMaxX || maxY = actualMaxY
            then Outer
            else Inner
    }

let corresponsingPortal portal =
    { portal with PortalType = if portal.PortalType = Inner then Outer else Inner }

let rec findPathsToPortals tiles state =
    match state.CurrentTile.Type, tiles |> Map.tryFind state.CurrentTile with
    | Passage, Some otherTiles ->
        otherTiles
        |> List.filter (fun tile -> state.MarkedTiles |> Set.contains tile |> not)
        |> List.collect (fun tile ->
            findPathsToPortals tiles {
                state with
                    CurrentTile = tile
                    CurrentPathLength = state.CurrentPathLength + 1
                    MarkedTiles = state.MarkedTiles |> Set.add tile
            })
    | PortalPart p1, Some ([ _; { Type = PortalPart p2 } as tile ] | [ { Type = PortalPart p2 } as tile; _ ])  ->
        let targetPortal = concatPortal (state.MaxX, state.MaxY) (state.CurrentTile, tile) (p1, p2)

        if targetPortal <> state.SourcePortal
        then [ targetPortal, state.CurrentPathLength ]
        else []
    | _ ->
        []

let constructMaze maxX maxY tiles =
    tiles
    |> Map.toList
    |> List.choose (fun (tile, tiles) -> match tiles with [ otherTile ] -> Some (tile, otherTile) | _ -> None)
    |> List.choose (fun (tile1, tile2) ->
        match tile1.Type, tile2.Type, tiles |> Map.tryFind tile2 with
        | PortalPart p1, PortalPart p2, Some ([ _; { Type = Passage } as tile ] | [ { Type = Passage } as tile; _ ]) ->
            let portal = concatPortal (maxX, maxY) (tile1, tile2) (p1, p2)
            Some <| initialPathState portal tile maxX maxY
        | _ ->
            None)
    |> List.map (fun state -> state.SourcePortal, findPathsToPortals tiles state)
    |> List.groupBy fst
    |> List.map (fun (portal, otherPortals) -> portal, otherPortals |> List.map snd |> List.concat)
    |> List.map (fun (portal, otherPortals) ->
        if portal.Label <> "AA" && portal.Label <> "ZZ"
        then portal, (portal |> corresponsingPortal, 0) :: otherPortals
        else portal, otherPortals)
    |> Map.ofList
    |> Maze

let linesToArray lines =
    lines
    |> Array.mapi (fun y -> Seq.mapi (fun x -> parseTileType >> Option.map (fun t -> { X = x; Y = y; Type = t })))
    |> array2D

let findAdjacentTiles (tiles : Tile option[,]) tile =
    let maxY = tiles |> Array2D.length1
    let maxX = tiles |> Array2D.length2

    [
        if tile.Y > 0 then
            tiles.[tile.Y - 1, tile.X]
        if tile.Y < (maxY - 1) then
            tiles.[tile.Y + 1, tile.X]
        if tile.X > 0 then
            tiles.[tile.Y, tile.X - 1]
        if tile.X < (maxX - 1) then
            tiles.[tile.Y, tile.X + 1]
    ]
    |> List.choose id

let arrayToMaze tiles =
    let maxY = (tiles |> Array2D.length1) - 1
    let maxX = (tiles |> Array2D.length2) - 1

    tiles
    |> Seq.cast<Tile option>
    |> Seq.choose id
    |> Seq.map (fun tile -> tile, tile |> findAdjacentTiles tiles)
    |> Map.ofSeq
    |> constructMaze maxX maxY

let getInput file =
    let lines = file |> File.ReadAllLines
    let maxLength = lines |> Array.maxBy String.length |> String.length

    lines
    |> Array.map (fun line -> line.PadRight(maxLength, ' '))
    |> linesToArray
    |> arrayToMaze

let calculateAdjacentPortalLevel useLevels state portal =
    if useLevels && portal.Label = state.CurrentPortal.Label then
        if state.CurrentPortal.PortalType = Inner
        then state.CurrentLevel + 1
        else state.CurrentLevel - 1
    else
        state.CurrentLevel

let calculateNewShortestPaths useLevels maze start end' state (portal, length) =
    let currentLength =
        state.ShortestPaths
        |> Map.tryFind (portal, portal |> calculateAdjacentPortalLevel useLevels state)
        |> Option.defaultValue Int32.MaxValue

    let newShortestPaths =
        if useLevels &&
            state.CurrentPortal.PortalType = Inner &&
            state.ShortestPaths |> Map.keys |> Seq.forall (snd >> fun level -> level <> state.CurrentLevel + 1) then
            maze
            |> Map.keys
            |> Seq.toList
            |> List.filter (fun portal -> portal.Label <> start.Label && portal.Label <> end'.Label)
            |> List.fold
                (fun paths portal -> paths |> Map.add (portal, state.CurrentLevel + 1) Int32.MaxValue)
                state.ShortestPaths
        else
            state.ShortestPaths

    newShortestPaths
    |> Map.tryFind (state.CurrentPortal, state.CurrentLevel)
    |> Option.map (fun pathLength ->
        let level = portal |> calculateAdjacentPortalLevel useLevels state
        let length = min currentLength (length + pathLength)
        { state with ShortestPaths = newShortestPaths |> Map.add (portal, level) length })
    |> Option.defaultValue state

let filterPortalsByLevel useLevels start end' state portals =
    if not useLevels then
        portals
    else
        portals
        |> List.filter (fst >> fun portal ->
            portal.Label = state.CurrentPortal.Label ||
            state.CurrentLevel <> 0 ||
            portal = end' ||
            portal.PortalType = Inner)
        |> List.filter (fst >> fun portal ->
            state.CurrentLevel = 0 || (portal <> start && portal <> end'))

let findShortestPathLength start end' useLevels (Maze maze) =
    let rec findShortestPathLength' state =
        if state.CurrentPortal = end' && state.CurrentLevel = 0 then
            state.ShortestPaths |> Map.tryFind (end', 0) |> Option.defaultValue Int32.MaxValue
        else
            let newState =
                maze
                |> Map.tryFind state.CurrentPortal
                |> Option.defaultValue []
                |> List.filter (fst >> fun portal ->
                    state.VisitedPortals
                    |> Set.contains (portal, portal |> calculateAdjacentPortalLevel useLevels state)
                    |> not)
                |> filterPortalsByLevel useLevels start end' state
                |> List.fold (calculateNewShortestPaths useLevels maze start end') state

            let newVisitedPortals =
                state.VisitedPortals
                |> Set.add (state.CurrentPortal, state.CurrentLevel)

            let newPortal, newLevel =
                newState.ShortestPaths
                |> Map.toList
                |> List.filter (fst >> fun portalAndLevel -> newVisitedPortals |> Set.contains portalAndLevel |> not)
                |> List.minBy snd
                |> fst

            { newState with CurrentPortal = newPortal; CurrentLevel = newLevel; VisitedPortals = newVisitedPortals }
            |> findShortestPathLength'

    let shortestPaths =
        maze
        |> Map.keys
        |> Seq.toList
        |> List.map (fun portal -> (portal, 0), if portal = start then 0 else Int32.MaxValue)
        |> Map.ofList

    { CurrentPortal = start; CurrentLevel = 0; ShortestPaths = shortestPaths; VisitedPortals = Set.empty }
    |> findShortestPathLength'

let mainPortal label =
    { Label = label; PortalType = Outer }

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let maze = file |> getInput

        let length = maze |> findShortestPathLength (mainPortal "AA") (mainPortal "ZZ") false
        printfn "%d" (length - 1)

        let length = maze |> findShortestPathLength (mainPortal "AA") (mainPortal "ZZ") true
        printfn "%d" (length - 1)

        0
    | _ ->
        printfn "Usage: Aoc.Day20 file"
        1

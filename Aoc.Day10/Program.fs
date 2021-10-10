open System
open System.IO

type Cell = Empty | Asteroid

let parseCell = function '.' -> Some Empty | '#' -> Some Asteroid | _ -> None

let rec gcd (a, b) = if b = 0 then abs a else gcd (b, a % b)

let simplify (a, b) =
    let gcd = gcd (a, b)
    (a / gcd, b / gcd)

let rec isDetectable (sourceX, sourceY) (targetX, targetY) (stepX, stepY) (map : Cell[,]) =
    let currentX = sourceX + stepX
    let currentY = sourceY + stepY

    currentX = targetX && currentY = targetY ||
    map.[currentY, currentX] = Empty && isDetectable (currentX, currentY) (targetX, targetY) (stepX, stepY) map

let calculateDirectAsteroids (x, y) map =
    map
    |> Array2D.mapi (fun currentY currentX -> function
        | Empty -> false
        | Asteroid ->
            (not (x = currentX && y = currentY)) &&
            isDetectable (x, y) (currentX, currentY) (simplify (currentX - x, currentY - y)) map)
    |> Seq.cast<bool>
    |> Seq.filter id
    |> Seq.length

let calculateGreatestDirectAsteroids map =
    map
    |> Array2D.mapi (fun y x -> function Empty -> (x, y, -1) | Asteroid -> (x, y, calculateDirectAsteroids (x, y) map))
    |> Seq.cast<int * int * int>
    |> Seq.maxBy (fun (_, _, num) -> num)

let clockwiseAngle (originX, originY) (x, y) =
    let offsetX = x - originX
    let offsetY = -1 * (y - originY)

    let angle = Math.Atan2(float offsetY, float offsetX)

    if Math.PI >= angle && angle > (Math.PI / 2.0)
    then angle - Math.PI * 2.0
    else angle

let destroyAsteroids (x, y) map =
    map
    |> Array2D.mapi (fun y x cell -> (x, y, cell))
    |> Seq.cast<int * int * Cell>
    |> List.ofSeq
    |> List.choose (fun (x, y, cell) -> match cell with Asteroid -> Some (x, y) | Empty -> None)
    |> List.filter (fun (currentX, currentY) -> not (x = currentX && y = currentY))
    |> List.filter (fun (currentX, currentY) ->
        isDetectable (x, y) (currentX, currentY) (simplify (currentX - x, currentY - y)) map)
    |> List.sortByDescending (clockwiseAngle (x, y))

let rec findDestroyedAsteroid (x, y) num map =
    let destroyed = destroyAsteroids (x, y) map
    let numDestroyed = destroyed |> List.length

    if num <= numDestroyed then
        destroyed |> List.item (num - 1)
    else
        let nextMap = map |> Array2D.mapi (fun y x cell -> if destroyed |> List.contains (x, y) then Empty else cell)
        nextMap |> findDestroyedAsteroid (x, y) (num - numDestroyed)

let getInput =
    File.ReadAllLines
    >> Array.map (Seq.map parseCell >> Seq.choose id)
    >> array2D

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let map = file |> getInput

        let (x, y, num) = map |> calculateGreatestDirectAsteroids

        printfn "%d" num

        let (x, y) = map |> findDestroyedAsteroid (x, y) 200

        printfn "%d" (x * 100 + y)

        0
    | _ ->
        printfn "Usage: Aoc.Day10 file"
        1

open System
open System.IO
open System.Text.RegularExpressions

type Point = {
    X : int
    Y : int
    Z : int
}

type Velocity = {
    ToX : int
    ToY : int
    ToZ : int
}

type Moon = {
    Location : Point
    Velocity : Velocity
}

let (|Int|_|) (str : string) =
    let success, num = Int32.TryParse(str)
    if success then Some num else None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some <| List.tail [ for g in m.Groups -> g.Value ]
    else None

let parseCoordinates =
    function
    | Regex "<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>" [ Int x; Int y; Int z ] -> Some { X = x; Y = y; Z = z }
    | _ -> None

let addVelocity { ToX = toX1; ToY = toY1; ToZ = toZ1 } { ToX = toX2; ToY = toY2; ToZ = toZ2 } =
    { ToX = toX2 + toX1; ToY = toY2 + toY1; ToZ = toZ2 + toZ1 }

let updateAsix coordinate1 coordinate2 =
    if coordinate1 = coordinate2
    then 0
    elif coordinate1 < coordinate2
    then 1
    else -1

let calculateGravity moon1 moon2 =
    let location1 = moon1.Location
    let location2 = moon2.Location

    {
        ToX = updateAsix location1.X location2.X
        ToY = updateAsix location1.Y location2.Y
        ToZ = updateAsix location1.Z location2.Z
    }

let updateVelocity moon update =
    { moon with Velocity = update moon.Velocity }

let updateDirections moons =
    moons
    |> List.allPairs moons
    |> List.filter (fun (a, b) -> a <> b)
    |> List.map (fun (moon1, moon2) -> moon1, addVelocity (calculateGravity moon1 moon2))
    |> List.groupBy fst
    |> List.map (fun (moon, moves) -> moves |> List.map snd |> List.fold updateVelocity moon)

let updateLocation moon =
    let location = moon.Location
    let velocity = moon.Velocity
    { moon with
        Location = { X = location.X + velocity.ToX; Y = location.Y + velocity.ToY; Z = location.Z + velocity.ToZ } }

let updateLocations moons =
    moons |> List.map updateLocation

let update = updateDirections >> updateLocations

let emulateMoonSystem num =
    let rec emulateMoonSystem' currentNum moons =
        if num = currentNum
        then moons
        else moons |> update |> emulateMoonSystem' (currentNum + 1)

    emulateMoonSystem' 0

let initMoon location =
    { Location = location; Velocity = { ToX = 0; ToY = 0; ToZ = 0 } }

let calculateEnergy moon =
    let location = moon.Location
    let velocity = moon.Velocity

    let potentialEnergy = abs location.X + abs location.Y + abs location.Z
    let kineticEnergy = abs velocity.ToX + abs velocity.ToY + abs velocity.ToZ

    potentialEnergy * kineticEnergy

let hash fields moons =
    moons
    |> List.collect fields
    |> List.map string
    |> String.concat ":"

let rec gcd (a, b) = if b = 0L then abs a else gcd (b, a % b)

let lcm (a : int64, b : int64) = int64 <| ((bigint a) * (bigint b)) / bigint (gcd (a, b))

let findRepetition moons =
    let rec findRepeatingStep num hash hashes moons =
        let newHash = hash moons
        if hashes |> Set.contains newHash
        then num
        else findRepeatingStep (num + 1L) hash (hashes |> Set.add newHash) (moons |> update)

    let byX = findRepeatingStep 0L (hash <| fun moon -> [ moon.Location.X; moon.Velocity.ToX ]) Set.empty moons
    let byY = findRepeatingStep 0L (hash <| fun moon -> [ moon.Location.Y; moon.Velocity.ToY ]) Set.empty moons
    let byZ = findRepeatingStep 0L (hash <| fun moon -> [ moon.Location.Z; moon.Velocity.ToZ ]) Set.empty moons

    lcm (lcm (byX, byY), byZ)

let getInput =
    File.ReadAllLines
    >> List.ofArray
    >> List.choose parseCoordinates

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let moons =
            file
            |> getInput
            |> List.map initMoon

        let newMoons = moons |> emulateMoonSystem 1000

        newMoons
        |> List.sumBy calculateEnergy
        |> printfn "%d"

        moons
        |> findRepetition
        |> printfn "%d"

        0
    | _ ->
        printfn "Usage: Aoc.Day12 file"
        1

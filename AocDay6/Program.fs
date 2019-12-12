module AocDay6.Program

open System.IO

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "The input file hasn't been specified"
    else
        maybe {
            let orbits =
                argv.[0]
                |> File.ReadAllLines
                |> Array.toList
                |> List.map (split ')')
                |> List.map exactlyTwo
                |> List.choose id
                |> List.map Orbit.ofPair

            let findParent tag =
                orbits
                |> List.filter (fun orbit -> (orbit |> Orbit.smallerObject) = tag)
                |> List.tryExactlyOne
                |> Option.map Orbit.biggerObject

            let! from = findParent "YOU"
            let! to' = findParent "SAN"

            orbits
            |> OrbitTree.ofOrbits
            |> Option.map (OrbitTree.numTransfers from to')
            |> Option.defaultValue 0
            |> printfn "%i"
        } |> ignore
    
    0

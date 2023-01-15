#load "Input.fsx"
open Input

let moves =
    [
        -1, 0, 0
        1, 0, 0
        0, -1, 0
        0, 1, 0
        0, 0, -1
        0, 0, 1
    ]

let droplet = Set.ofList input

let surfaceArea cluster =
    cluster
    |> Set.toList
    |> List.map (fun (x, y, z) ->
        moves
        |> List.filter (fun (dx, dy, dz) ->
            not (Set.contains (x + dx, y + dy, z + dz) cluster)
        )
        |> List.length
    )
    |> List.sum

let xs = input |> List.map (fun (x, _, _) -> x)
let ys = input |> List.map (fun (_, y, _) -> y)
let zs = input |> List.map (fun (_, _, z) -> z)

let minX = xs |> List.min
let maxX = xs |> List.max
let minY = ys |> List.min
let maxY = ys |> List.max
let minZ = zs |> List.min
let maxZ = zs |> List.max

let xRange = [minX..maxX]
let yRange = [minY..maxY]
let zRange = [minZ..maxZ]

let solidCube =
    xRange
    |> List.collect (fun x ->
        yRange
        |> List.collect (fun y ->
            zRange
            |> List.map (fun z -> x, y, z)
        )
    )
    |> Set.ofList

let air =
    Set.difference solidCube droplet

let rec findAirCluster (x, y, z) remainingAir =
    let candidates =
        moves
        |> List.map (fun (dx, dy, dz) ->
            x + dx, y + dy, z + dz
        )
        |> List.filter (fun coordinate ->
            Set.contains coordinate remainingAir
        )

    let startCluster = (x, y, z) :: candidates |> Set.ofList

    candidates
    |> List.fold
        (fun (accCluster, accAir) coordinate ->
            let newCluster, newAir =
                findAirCluster coordinate accAir
            Set.union accCluster newCluster, newAir
        )
        (startCluster, Set.difference remainingAir startCluster)

let rec findAllAirClusters remainingAir =
    if Set.isEmpty remainingAir then
        []
    else
        let coordinate = Set.minElement remainingAir
        let cluster, newAir = findAirCluster coordinate remainingAir
        cluster :: findAllAirClusters newAir

let airPockets =
    air
    |> findAllAirClusters
    |> List.filter (
        Set.toList
        >> List.forall (fun (x, y, z) ->
            not (x = minX || x = maxX || y = minY || y = maxY || z = minZ || z = maxZ)
        )
    )

let dropletArea = surfaceArea droplet

let airPocketsArea =
    airPockets
    |> List.map surfaceArea
    |> List.sum

printfn "a: %i" dropletArea
printfn "b: %i" (dropletArea - airPocketsArea)

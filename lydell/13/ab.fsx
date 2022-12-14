#load "Input.fsx"
open Input

let rec elfCompare x y =
    match x, y with
    | I x, I y -> compare x y
    | L x, L y ->
        let rec loop x y =
            match x, y with
            | [], [] -> 0
            | [], _ -> -1
            | _, [] -> 1
            | x::xs, y::ys ->
                match elfCompare x y with
                | 0 -> loop xs ys
                | comparison -> comparison
        loop x y
    | L x, I y -> elfCompare (L x) (L [I y])
    | I x, L y -> elfCompare (L [I x]) (L y)

let solutionA =
    input
    |> List.indexed
    |> List.filter (fun (i, (x, y)) -> elfCompare x y < 0)
    |> List.map (fun (i, _) -> i + 1)
    |> List.sum

let dividerPacket1 = L [L [I 2]]
let dividerPacket2 = L [L [I 6]]

let sorted =
    input
    |> List.collect (fun (x, y) -> [x; y])
    |> List.append [dividerPacket1; dividerPacket2]
    |> List.sortWith elfCompare

let solutionB =
    (List.findIndex ((=) dividerPacket1) sorted + 1) * (List.findIndex ((=) dividerPacket2) sorted + 1)

printfn "a: %i" solutionA
printfn "b: %i" solutionB

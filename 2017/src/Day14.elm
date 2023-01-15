module Day14 exposing (..)

import Array
import KnotHash
import Matrix exposing (Matrix)
import Set exposing (Set)


output : () -> ( String, String )
output () =
    ( input |> makeGrid |> countY |> toString
    , input |> makeGrid |> countClusters |> toString
    )


type Binary
    = N
    | Y


hexDigitToBinary : Char -> Maybe ( Binary, Binary, Binary, Binary )
hexDigitToBinary char =
    case char of
        '0' ->
            Just ( N, N, N, N )

        '1' ->
            Just ( N, N, N, Y )

        '2' ->
            Just ( N, N, Y, N )

        '3' ->
            Just ( N, N, Y, Y )

        '4' ->
            Just ( N, Y, N, N )

        '5' ->
            Just ( N, Y, N, Y )

        '6' ->
            Just ( N, Y, Y, N )

        '7' ->
            Just ( N, Y, Y, Y )

        '8' ->
            Just ( Y, N, N, N )

        '9' ->
            Just ( Y, N, N, Y )

        'a' ->
            Just ( Y, N, Y, N )

        'b' ->
            Just ( Y, N, Y, Y )

        'c' ->
            Just ( Y, Y, N, N )

        'd' ->
            Just ( Y, Y, N, Y )

        'e' ->
            Just ( Y, Y, Y, N )

        'f' ->
            Just ( Y, Y, Y, Y )

        _ ->
            Nothing


makeGrid : String -> List (List Binary)
makeGrid salt =
    List.range 0 127
        |> List.map (makeRow salt)


makeRow : String -> Int -> List Binary
makeRow salt int =
    salt
        ++ "-"
        ++ toString int
        |> KnotHash.hash256
        |> String.toList
        |> List.filterMap hexDigitToBinary
        |> List.concatMap (\( a, b, c, d ) -> [ a, b, c, d ])


countY : List (List Binary) -> Int
countY =
    List.map (List.filter ((==) Y) >> List.length)
        >> List.sum


countClusters : List (List Binary) -> Maybe Int
countClusters =
    Matrix.fromList
        >> Maybe.map (findAllClusters >> List.length)


findAllClusters : Matrix Binary -> List (Set ( Int, Int ))
findAllClusters matrix =
    let
        addCluster coords clusters =
            if List.any (Set.member coords) clusters then
                clusters
            else
                let
                    newCluster =
                        findCluster matrix coords
                in
                if newCluster == Set.empty then
                    clusters
                else
                    newCluster :: clusters
    in
    matrix
        |> Matrix.toIndexedArray
        |> Array.map Tuple.first
        |> Array.foldl addCluster []


findCluster : Matrix Binary -> ( Int, Int ) -> Set ( Int, Int )
findCluster matrix coords =
    findClusterHelper matrix coords Set.empty


findClusterHelper :
    Matrix Binary
    -> ( Int, Int )
    -> Set ( Int, Int )
    -> Set ( Int, Int )
findClusterHelper matrix ( x, y ) cluster =
    if Set.member ( x, y ) cluster then
        cluster
    else
        case Matrix.get x y matrix of
            Just Y ->
                let
                    neighbourCoords =
                        [ ( x, y - 1 )
                        , ( x + 1, y )
                        , ( x, y + 1 )
                        , ( x - 1, y )
                        ]
                in
                List.foldl
                    (findClusterHelper matrix)
                    (Set.insert ( x, y ) cluster)
                    neighbourCoords

            _ ->
                cluster


input : String
input =
    "ljoxqyyw"

module Day14 exposing (..)

import Day14Input exposing (input)
import KnotHash


output : () -> ( String, String )
output () =
    ( input |> makeGrid |> countY |> toString
    , ""
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

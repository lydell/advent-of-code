module Day5 exposing (..)

import Binary
import Day5Input exposing (puzzleInput)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Set exposing (Set)


parse : String -> List Int
parse =
    String.trim
        >> String.lines
        >> List.map
            (String.toList
                >> List.map (\char -> char == 'B' || char == 'R')
                >> Binary.fromBooleans
                >> Binary.toDecimal
            )


solution1 : String -> Maybe Int
solution1 =
    parse >> List.maximum


solution2 : String -> List Int
solution2 input =
    let
        ids =
            parse input

        min =
            List.minimum ids |> Maybe.withDefault 0

        max =
            List.maximum ids |> Maybe.withDefault 0

        all =
            List.range min max
                |> Set.fromList
    in
    ids
        |> Set.fromList
        |> Set.diff all
        |> Set.toList


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput |> Maybe.toList)
        , showResult (solution2 puzzleInput)
        ]


showResult : List Int -> Html msg
showResult result =
    Html.output []
        [ Html.text
            (if List.isEmpty result then
                "No solutions found"

             else
                result |> List.map String.fromInt |> String.join ", "
            )
        ]

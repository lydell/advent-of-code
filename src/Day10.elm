module Day10 exposing (..)

import Day10Input exposing (puzzleInput)
import Html exposing (Html)
import LineParser
import List.Extra as List
import Set


parse : String -> Result String (List Int)
parse =
    LineParser.parse (String.toInt >> Result.fromMaybe "Not a number.")


solution1 : String -> Result String Int
solution1 =
    parse >> Result.andThen solve1


solve1 : List Int -> Result String Int
solve1 adapters =
    let
        diffs =
            getDiffs adapters

        ones =
            diffs |> List.filter ((==) 1) |> List.length

        threes =
            diffs |> List.filter ((==) 3) |> List.length

        set =
            diffs |> List.filter ((/=) 2) |> Set.fromList
    in
    if set == Set.fromList [ 1, 3 ] then
        Ok (ones * threes)

    else
        Err
            ("Expected only 1, 2, 3 as differences, but got: "
                ++ (set |> Set.toList |> List.map String.fromInt |> String.join ", ")
            )


getDiffs : List number -> List number
getDiffs adapters =
    let
        chargingOutlet =
            0

        deviceAdapter =
            case List.maximum adapters of
                Just jolt ->
                    [ jolt + 3 ]

                Nothing ->
                    []

        sorted =
            List.sort adapters
    in
    List.map2 (-)
        (sorted ++ deviceAdapter)
        (chargingOutlet :: sorted)


solution2 : String -> Result String Int
solution2 =
    parse >> Result.map solve2


solve2 : List Int -> Int
solve2 adapters =
    let
        diffs =
            getDiffs adapters

        groups =
            List.group diffs
    in
    groups
        |> List.filter (Tuple.first >> (==) 1)
        |> List.map
            (\( _, items ) ->
                -- This many 1s in a row gives that many variations (calculated by hand).
                case List.length items + 1 of
                    1 ->
                        1

                    2 ->
                        2

                    3 ->
                        4

                    4 ->
                        7

                    -- My input never gets more than 4 1s in a row.
                    _ ->
                        0
            )
        |> List.product


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
        , showResult (solution2 puzzleInput)
        ]


showResult : Result String Int -> Html msg
showResult result =
    Html.output []
        [ Html.text
            (case result of
                Ok int ->
                    String.fromInt int

                Err error ->
                    error
            )
        ]

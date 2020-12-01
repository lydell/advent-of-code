module Day1 exposing (..)

import Day1Input exposing (puzzleInput)
import Html exposing (Html)
import Set exposing (Set)


parse : String -> List Int
parse =
    String.trim
        >> String.split "\n"
        >> List.filterMap String.toInt


solution1 : String -> Maybe Int
solution1 =
    parse >> solve1 2020


solve1 : Int -> List Int -> Maybe Int
solve1 target list =
    solve1Helper target (Set.fromList list) list
        |> Maybe.map (\( a, b ) -> a * b)


solve1Helper : Int -> Set Int -> List Int -> Maybe ( Int, Int )
solve1Helper target set list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            let
                needle =
                    target - first
            in
            if Set.member needle set then
                Just ( first, needle )

            else
                solve1Helper target set rest


solution2 : String -> Maybe Int
solution2 =
    parse >> solve2 2020


solve2 : Int -> List Int -> Maybe Int
solve2 target list =
    solve2Helper target (Set.fromList list) list
        |> Maybe.map (\( a, b, c ) -> a * b * c)


solve2Helper : Int -> Set Int -> List Int -> Maybe ( Int, Int, Int )
solve2Helper target set list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case solve1Helper (target - first) set list of
                Just ( a, b ) ->
                    Just ( a, b, target - a - b )

                Nothing ->
                    solve2Helper target set rest


main : Html Never
main =
    Html.div []
        [ Html.div []
            [ Html.text
                (case solution1 puzzleInput of
                    Just answer ->
                        String.fromInt answer

                    Nothing ->
                        "Found nothing."
                )
            ]
        , Html.div []
            [ Html.text
                (case solution2 puzzleInput of
                    Just answer ->
                        String.fromInt answer

                    Nothing ->
                        "Found nothing."
                )
            ]
        ]

module Day1 exposing (..)

import Day1Input exposing (puzzleInput)
import Html exposing (Html)
import LineParser
import Set exposing (Set)


parse : String -> Result String (List Int)
parse =
    LineParser.parse (String.toInt >> Result.fromMaybe "String.toInt failed")


solution1 : String -> Result String Int
solution1 =
    parse >> Result.andThen (solve1 2020 >> Result.fromMaybe "No solution found")


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


solution2 : String -> Result String Int
solution2 =
    parse >> Result.andThen (solve2 2020 >> Result.fromMaybe "No solution found")


solve2 : Int -> List Int -> Maybe Int
solve2 target list =
    solve2Helper target list
        |> Maybe.map (\( a, b, c ) -> a * b * c)


solve2Helper : Int -> List Int -> Maybe ( Int, Int, Int )
solve2Helper target list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case solve1Helper (target - first) (Set.fromList rest) rest of
                Just ( a, b ) ->
                    Just ( a, b, target - a - b )

                Nothing ->
                    solve2Helper target rest


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

module Day13 exposing (..)

import Day13Input exposing (puzzleInput)
import Html exposing (Html)
import LineParser
import List.Extra as List


type BusId
    = X
    | Id Int


parse : String -> Result String ( Int, List BusId )
parse input =
    case String.lines (String.trim input) of
        [ first, second ] ->
            Result.map2 Tuple.pair
                (String.toInt first |> Result.fromMaybe "First line is not a number.")
                (LineParser.parseGeneral "Comma-separated item"
                    identity
                    (\item ->
                        case item of
                            "x" ->
                                Ok X

                            _ ->
                                String.toInt item
                                    |> Result.fromMaybe "Expected 'x' or a number."
                                    |> Result.map Id
                    )
                    (String.split "," second)
                )

        lines ->
            Err
                ("Expected two lines of input but got "
                    ++ String.fromInt (List.length lines)
                    ++ ": "
                    ++ input
                )


solution1 : String -> Result String Int
solution1 =
    parse >> Result.andThen (solve1 >> Result.fromMaybe "No solution found.")


solve1 : ( Int, List BusId ) -> Maybe Int
solve1 ( estimate, ids ) =
    ids
        |> List.filterMap
            (\busId ->
                case busId of
                    X ->
                        Nothing

                    Id id ->
                        Just ( id, ceiling (toFloat estimate / toFloat id) * id )
            )
        |> List.minimumBy Tuple.second
        |> Maybe.map (\( id, minutes ) -> (minutes - estimate) * id)


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
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

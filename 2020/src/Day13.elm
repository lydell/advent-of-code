module Day13 exposing (..)

import Html exposing (Html)
import Html.Attributes
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


solve1 : Int -> List BusId -> Maybe Int
solve1 estimate ids =
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


solve2 : Int -> Int -> List ( Int, Int ) -> List ( Int, Int ) -> Int
solve2 startTimestamp inc done rest =
    case rest of
        [] ->
            startTimestamp

        head :: tail ->
            let
                current =
                    head :: done

                timestamp1 =
                    findTimestamp startTimestamp inc current

                timestamp2 =
                    findTimestamp (timestamp1 + inc) inc current

                nextInc =
                    timestamp2 - timestamp1
            in
            solve2 timestamp1 nextInc current tail


findTimestamp : Int -> Int -> List ( Int, Int ) -> Int
findTimestamp startTimestamp inc indexedIds =
    let
        isMatch =
            indexedIds
                |> List.all
                    (\( index, id ) ->
                        startTimestamp + index |> modBy id |> (==) 0
                    )
    in
    if isMatch then
        startTimestamp

    else
        findTimestamp (startTimestamp + inc) inc indexedIds


main : Html Never
main =
    case parse puzzleInput of
        Ok ( estimate, busIds ) ->
            let
                indexedIds =
                    busIds
                        |> List.indexedMap Tuple.pair
                        |> List.filterMap
                            (\( index, busId ) ->
                                case busId of
                                    X ->
                                        Nothing

                                    Id id ->
                                        Just ( index, id )
                            )
            in
            Html.div []
                [ showResult (solve1 estimate busIds |> Result.fromMaybe "No solution found.")
                , Html.output []
                    [ Html.text (String.fromInt (solve2 0 1 [] indexedIds)) ]
                , viewRange 0 4000 indexedIds
                ]

        Err error ->
            Html.text error


viewRange : Int -> Int -> List ( Int, Int ) -> Html msg
viewRange from to indexedIds =
    Html.table [ Html.Attributes.style "margin-top" "2em" ]
        (Html.tbody []
            [ Html.tr []
                (Html.th []
                    [ Html.text "t" ]
                    :: (indexedIds
                            |> List.map
                                (\( index, id ) ->
                                    Html.th []
                                        [ Html.text
                                            ("["
                                                ++ String.fromInt index
                                                ++ ":"
                                                ++ String.fromInt id
                                                ++ "]"
                                            )
                                        ]
                                )
                       )
                )
            ]
            :: (List.range from to
                    |> List.map
                        (\t ->
                            Html.tr []
                                (Html.th []
                                    [ Html.text (String.fromInt t) ]
                                    :: (indexedIds
                                            |> List.map
                                                (\( _, id ) ->
                                                    Html.td []
                                                        (if t |> modBy id |> (==) 0 then
                                                            [ Html.span [ Html.Attributes.title (String.fromInt id) ]
                                                                [ Html.text "D" ]
                                                            ]

                                                         else
                                                            []
                                                        )
                                                )
                                       )
                                )
                        )
               )
        )


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


shortInput : String
shortInput =
    """
1
1789,37,47,1889
"""


puzzleInput : String
puzzleInput =
    """
1008833
19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,643,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,x,x,x,x,23,x,x,x,x,x,x,x,509,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29
"""

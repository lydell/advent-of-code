module Day15 exposing (..)

import Day15Input exposing (puzzleInput)
import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra as List


solution1 : List Int -> Result String Int
solution1 input =
    case List.last input of
        Nothing ->
            Err "Empty input."

        Just last ->
            input
                |> List.indexedMap (\index int -> ( int, ( index + 1, Nothing ) ))
                |> Dict.fromList
                |> solve1 (List.length input + 1) 2020 last
                |> Ok


solve1 : Int -> Int -> Int -> Dict Int ( Int, Maybe Int ) -> Int
solve1 currentTurn target last dict =
    if currentTurn > target then
        last

    else
        case Dict.get last dict of
            Just ( turn, Just turn2 ) ->
                let
                    next =
                        turn2 - turn
                in
                solve1 (currentTurn + 1) target next (updateDict currentTurn next dict)

            _ ->
                let
                    next =
                        0
                in
                solve1 (currentTurn + 1) target next (updateDict currentTurn next dict)


updateDict : Int -> Int -> Dict Int ( Int, Maybe Int ) -> Dict Int ( Int, Maybe Int )
updateDict currentTurn next =
    Dict.update next
        (\maybe ->
            case maybe of
                Just ( _, Just turn ) ->
                    Just ( turn, Just currentTurn )

                Just ( turn, Nothing ) ->
                    Just ( turn, Just currentTurn )

                Nothing ->
                    Just ( currentTurn, Nothing )
        )


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


shortInput : List number
shortInput =
    [ 0, 3, 6 ]

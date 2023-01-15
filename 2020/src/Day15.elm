-- Warning: Part 2 took almost 3 minutes to complete on my machine!


module Day15 exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra as List


solution : Int -> List Int -> Result String Int
solution target input =
    case List.last input of
        Nothing ->
            Err "Empty input."

        Just last ->
            input
                |> List.indexedMap (\index int -> ( int, ( index + 1, Nothing ) ))
                |> Dict.fromList
                |> solve (List.length input + 1) target last
                |> Ok


solve : Int -> Int -> Int -> Dict Int ( Int, Maybe Int ) -> Int
solve currentTurn target last dict =
    let
        _ =
            if currentTurn |> modBy 1000000 |> (==) 0 then
                Debug.log "currentTurn" currentTurn

            else
                0
    in
    if currentTurn > target then
        last

    else
        case Dict.get last dict of
            Just ( turn, Just turn2 ) ->
                let
                    next =
                        turn2 - turn
                in
                solve (currentTurn + 1) target next (updateDict currentTurn next dict)

            _ ->
                let
                    next =
                        0
                in
                solve (currentTurn + 1) target next (updateDict currentTurn next dict)


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
        [ showResult (solution 2020 puzzleInput)
        , showResult (solution 30000000 puzzleInput)
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


puzzleInput : List Int
puzzleInput =
    [ 2, 0, 1, 9, 5, 19 ]

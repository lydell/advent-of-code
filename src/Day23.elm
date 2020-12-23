module Day23 exposing (..)

import Day23Input exposing (puzzleInput)
import Html exposing (Html)


solution1 : List Int -> String
solution1 cups =
    List.repeat 100 ()
        |> List.foldl (always doOneRound) cups
        |> getOrderString


getOrderString : List Int -> String
getOrderString cups =
    let
        ( before, _, after ) =
            getDestination 1 cups
    in
    after ++ before |> List.map String.fromInt |> String.join ""


doOneRound : List Int -> List Int
doOneRound cups =
    case cups of
        first :: second :: third :: fourth :: rest ->
            let
                ( before, destination, after ) =
                    getDestination (first - 1) rest
            in
            before ++ [ destination, second, third, fourth ] ++ after ++ [ first ]

        _ ->
            cups


getDestination : Int -> List Int -> ( List Int, Int, List Int )
getDestination target rest =
    getDestinationHelper target [] rest


getDestinationHelper : Int -> List Int -> List Int -> ( List Int, Int, List Int )
getDestinationHelper target left right =
    case right of
        first :: rest ->
            if first == target then
                ( List.reverse left, first, rest )

            else
                getDestinationHelper target (first :: left) rest

        [] ->
            let
                nextTarget =
                    if target <= 1 then
                        List.maximum left |> Maybe.withDefault -1

                    else
                        target - 1
            in
            getDestinationHelper nextTarget [] (List.reverse left)


main : Html Never
main =
    Html.div []
        [ Html.text (solution1 puzzleInput)
        ]


showResult : Result String String -> Html msg
showResult result =
    Html.output []
        [ Html.text
            (case result of
                Ok string ->
                    string

                Err error ->
                    error
            )
        ]


shortInput : List Int
shortInput =
    [ 3, 8, 9, 1, 2, 5, 4, 6, 7 ]

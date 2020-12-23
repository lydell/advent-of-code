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

                _ =
                    Debug.log "dest" ( List.length before, destination, List.length after )
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


solution2 : List Int -> String
solution2 initialCups =
    let
        max =
            initialCups
                |> List.maximum
                |> Maybe.withDefault 0

        cups =
            initialCups ++ List.range (max + 1) 1000000
    in
    List.repeat 10000000 ()
        |> List.foldl (always doOneRound) cups
        |> getStarsProduct


getStarsProduct : List Int -> String
getStarsProduct cups =
    let
        ( before, _, after ) =
            getDestination 1 cups
    in
    case after of
        first :: second :: rest ->
            first * second |> String.fromInt

        _ ->
            "Too short list."


main : Html Never
main =
    Html.div []
        -- [ Html.text (solution1 puzzleInput)
        [ Html.text (solution2 puzzleInput)
        ]


shortInput : List Int
shortInput =
    [ 3, 8, 9, 1, 2, 5, 4, 6, 7 ]

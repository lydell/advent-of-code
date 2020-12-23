module Day23 exposing (..)

import Day23Input exposing (puzzleInput)
import Deque exposing (Deque)
import Html exposing (Html)


solution1 : List Int -> String
solution1 cups =
    let
        maximum =
            List.maximum cups |> Maybe.withDefault 0
    in
    List.repeat 100 ()
        |> List.foldl (always (doOneRound maximum)) (Deque.fromList cups)
        |> getOrderString


getOrderString : Deque Int -> String
getOrderString cups =
    let
        ( before, _, after ) =
            getDestination 1 1 cups
    in
    after
        |> append before
        |> Deque.map String.fromInt
        |> Deque.toList
        |> String.join ""


append : Deque a -> Deque a -> Deque a
append a b =
    Deque.append b a


doOneRound : Int -> Deque Int -> Deque Int
doOneRound maximum cups =
    case Deque.popFront cups of
        ( Just first, rest ) ->
            let
                pickedUp =
                    rest |> Deque.left 3

                ( before, destination, after ) =
                    getDestination maximum
                        (first - 1)
                        (Deque.dropLeft (Deque.length pickedUp) rest)

                -- _ =
                --     Debug.log "x" ( first, destination )
                -- _ =
                --     Debug.log "dest" ( Deque.length before, destination, Deque.length after )
            in
            before
                |> Deque.pushBack destination
                |> append pickedUp
                |> append after
                |> Deque.pushBack first

        ( Nothing, _ ) ->
            cups


getDestination : Int -> Int -> Deque Int -> ( Deque Int, Int, Deque Int )
getDestination maximum target rest =
    getDestinationHelper maximum target rest Deque.empty


getDestinationHelper : Int -> Int -> Deque Int -> Deque Int -> ( Deque Int, Int, Deque Int )
getDestinationHelper maximum target left right =
    case Deque.popBack left of
        ( Just last, rest ) ->
            if last == target then
                ( rest, last, right )

            else
                getDestinationHelper maximum target rest (right |> Deque.pushFront last)

        ( Nothing, _ ) ->
            let
                nextTarget =
                    if target <= 1 then
                        maximum

                    else
                        target - 1
            in
            getDestinationHelper maximum nextTarget right Deque.empty


solution2 : List Int -> String
solution2 initialCups =
    let
        max =
            initialCups
                |> List.maximum
                |> Maybe.withDefault 0

        maximum =
            1000000

        cups =
            (initialCups ++ List.range (max + 1) maximum)
                |> Deque.fromList
    in
    List.range 1 10000000
        |> List.foldl (log >> always (doOneRound maximum)) cups
        |> getStarsProduct


log n =
    if n - 1 |> modBy 10 |> (==) 0 then
        Debug.log "n" n

    else
        n


getStarsProduct : Deque Int -> String
getStarsProduct cups =
    let
        ( before, _, after ) =
            getDestination 1 1 cups
    in
    after
        |> Deque.left 2
        |> Deque.toList
        |> List.product
        |> String.fromInt


main : Html Never
main =
    Html.div []
        -- [ Html.text (solution1 puzzleInput)
        [ Html.text (solution2 puzzleInput)
        ]


shortInput : List Int
shortInput =
    [ 3, 8, 9, 1, 2, 5, 4, 6, 7 ]

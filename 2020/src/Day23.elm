-- Warning: Part 2 took like one and a half minutes to complete on my machine!


module Day23 exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import IntDict exposing (after)
import List.Extra


solution1 : Parsed -> Result String Int
solution1 { maximum, first, cups } =
    doRounds 100 maximum ( first, cups )
        |> Result.andThen getOrderInt


getOrderInt : Dict Int Int -> Result String Int
getOrderInt cups =
    getOrderIntHelper 1 cups []
        |> List.map String.fromInt
        |> String.join ""
        |> String.toInt
        |> Result.fromMaybe "Not an integer"


getOrderIntHelper : Int -> Dict Int Int -> List Int -> List Int
getOrderIntHelper current cups result =
    case Dict.get current cups of
        Just next ->
            if next == 1 then
                List.reverse result

            else
                getOrderIntHelper next cups (next :: result)

        Nothing ->
            List.reverse result


doRounds : Int -> Int -> ( Int, Dict Int Int ) -> Result String (Dict Int Int)
doRounds rounds maximum currentAndCups =
    case doOneRound maximum currentAndCups of
        Ok next ->
            if rounds > 1 then
                doRounds (rounds - 1) maximum next

            else
                Ok (Tuple.second next)

        Err message ->
            Err message


doOneRound : Int -> ( Int, Dict Int Int ) -> Result String ( Int, Dict Int Int )
doOneRound maximum ( current, cups ) =
    case getHead current cups of
        Ok ( next1, next2, ( next3, next4 ) ) ->
            let
                destination =
                    getDestination maximum current [ next1, next2, next3 ]
            in
            case Dict.get destination cups of
                Just afterDestination ->
                    let
                        nextCups =
                            cups
                                |> Dict.insert current next4
                                |> Dict.insert destination next1
                                |> Dict.insert next3 afterDestination
                    in
                    Ok ( next4, nextCups )

                Nothing ->
                    Err "destination not found"

        Err message ->
            Err message


getHead : Int -> Dict Int Int -> Result String ( Int, Int, ( Int, Int ) )
getHead current cups =
    case Dict.get current cups of
        Just next1 ->
            case Dict.get next1 cups of
                Just next2 ->
                    case Dict.get next2 cups of
                        Just next3 ->
                            case Dict.get next3 cups of
                                Just next4 ->
                                    Ok ( next1, next2, ( next3, next4 ) )

                                Nothing ->
                                    Err "next3 not found"

                        Nothing ->
                            Err "next2 not found"

                Nothing ->
                    Err "next1 not found"

        Nothing ->
            Err "current not found"


getDestination : Int -> Int -> List Int -> Int
getDestination maximum current pickedUp =
    let
        target =
            if current <= 1 then
                maximum

            else
                current - 1
    in
    if List.member target pickedUp then
        getDestination maximum target pickedUp

    else
        target


solution2 : Parsed -> Result String Int
solution2 { maximum, first, last, cups } =
    let
        newMaximum =
            1000000

        fullCups =
            List.range (maximum + 1) (newMaximum - 1)
                |> List.foldl (\n -> Dict.insert n (n + 1)) cups
                |> Dict.insert last (maximum + 1)
                |> Dict.insert newMaximum first
    in
    doRounds 10000000 newMaximum ( first, fullCups )
        |> Result.andThen getStarsProduct


getStarsProduct : Dict Int Int -> Result String Int
getStarsProduct cups =
    case Dict.get 1 cups of
        Just next1 ->
            case Dict.get next1 cups of
                Just next2 ->
                    Ok (next1 * next2)

                Nothing ->
                    Err "next1 not found"

        Nothing ->
            Err "1 not found"


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


puzzleInput : Parsed
puzzleInput =
    [ 1, 5, 6, 7, 9, 4, 8, 2, 3 ]
        |> parse


type alias Parsed =
    { maximum : Int
    , first : Int
    , last : Int
    , cups : Dict Int Int
    }


parse : List Int -> Parsed
parse numbers =
    case numbers of
        [] ->
            { maximum = 0
            , first = 0
            , last = 0
            , cups = Dict.empty
            }

        first :: rest ->
            let
                cups =
                    List.map2 Tuple.pair
                        (first :: rest)
                        (rest ++ [ first ])
                        |> List.foldl (\( from, to ) -> Dict.insert from to) Dict.empty
            in
            { maximum = numbers |> List.maximum |> Maybe.withDefault first
            , first = first
            , last = List.Extra.last rest |> Maybe.withDefault first
            , cups = cups
            }

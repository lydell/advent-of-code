module Day22 exposing (..)

import Day22Input exposing (puzzleInput)
import Deque exposing (Deque)
import Html exposing (Html)
import LineParser


parse : String -> Result String ( Deque Int, Deque Int )
parse input =
    case input |> String.trim |> String.split "\n\n" of
        [ first, second ] ->
            Result.map2 Tuple.pair
                (parseCards first |> Result.mapError ((++) "Player 1: "))
                (parseCards second |> Result.mapError ((++) "Player 2: "))

        parts ->
            Err ("Expected 2 parts but got " ++ String.fromInt (List.length parts))


parseCards : String -> Result String (Deque Int)
parseCards =
    String.lines
        >> List.drop 1
        >> String.join "\n"
        >> LineParser.parse (String.toInt >> Result.fromMaybe "Not a number.")
        >> Result.map Deque.fromList


solution1 : String -> Result String Int
solution1 =
    parse >> Result.map (play >> score)


score : Deque Int -> Int
score =
    Deque.toList
        >> List.reverse
        >> List.indexedMap (\index number -> number * (index + 1))
        >> List.sum


play : ( Deque Int, Deque Int ) -> Deque Int
play decks =
    let
        ( next1, next2 ) =
            playOneRound decks
    in
    if Deque.isEmpty next1 then
        next2

    else if Deque.isEmpty next2 then
        next1

    else
        play ( next1, next2 )


playOneRound : ( Deque Int, Deque Int ) -> ( Deque Int, Deque Int )
playOneRound ( player1, player2 ) =
    case ( Deque.popFront player1, Deque.popFront player2 ) of
        ( ( Just first1, rest1 ), ( Just first2, rest2 ) ) ->
            if first1 > first2 then
                ( rest1 |> Deque.pushBack first1 |> Deque.pushBack first2
                , rest2
                )

            else
                ( rest1
                , rest2 |> Deque.pushBack first2 |> Deque.pushBack first1
                )

        _ ->
            ( player1, player2 )


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

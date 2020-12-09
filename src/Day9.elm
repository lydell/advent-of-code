module Day9 exposing (..)

import Day9Input exposing (puzzleInput)
import Deque exposing (Deque)
import Html exposing (Html)
import LineParser


parse : String -> Result String (List Int)
parse =
    LineParser.parse (String.toInt >> Result.fromMaybe "Not a number.")


solution1 : String -> Result String Int
solution1 =
    parse >> Result.andThen (findFirstInvalid 25 >> Result.fromMaybe "No invalid number found.")


findFirstInvalid : Int -> List Int -> Maybe Int
findFirstInvalid preambleSize numbers =
    let
        preamble =
            List.take preambleSize numbers
                |> Deque.fromList

        rest =
            List.drop preambleSize numbers
    in
    findFirstInvalidHelper preamble rest


findFirstInvalidHelper : Deque Int -> List Int -> Maybe Int
findFirstInvalidHelper preamble numbers =
    case numbers of
        [] ->
            Nothing

        first :: rest ->
            let
                matching =
                    preamble
                        |> Deque.filter
                            (\number ->
                                let
                                    otherNumber =
                                        first - number
                                in
                                otherNumber > 0 && otherNumber /= number && Deque.member otherNumber preamble
                            )
            in
            if Deque.isEmpty matching then
                Just first

            else
                let
                    nextPreamble =
                        preamble
                            |> Deque.popFront
                            |> Tuple.second
                            |> Deque.pushBack first
                in
                findFirstInvalidHelper nextPreamble rest


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

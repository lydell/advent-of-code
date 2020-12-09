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


solution2 : String -> Result String Int
solution2 =
    parse
        >> Result.andThen
            (\numbers ->
                findFirstInvalid 25 numbers
                    |> Result.fromMaybe "No invalid number found."
                    |> Result.andThen
                        (findContiguous numbers
                            >> Result.fromMaybe "No contiguous sequence found."
                        )
                    |> Result.andThen
                        (findWeakness
                            >> Result.fromMaybe "No weakness found."
                        )
            )


findWeakness : List number -> Maybe number
findWeakness range =
    Maybe.map2 (+)
        (List.minimum range)
        (List.maximum range)


findContiguous : List Int -> Int -> Maybe (List Int)
findContiguous numbers target =
    case numbers of
        first :: second :: rest ->
            case findContiguousHelper target [ first, second ] rest of
                Just range ->
                    Just range

                Nothing ->
                    findContiguous (second :: rest) target

        _ ->
            Nothing


findContiguousHelper : Int -> List Int -> List Int -> Maybe (List Int)
findContiguousHelper target range numbers =
    case compare (List.sum range) target of
        LT ->
            case numbers of
                [] ->
                    Nothing

                first :: rest ->
                    findContiguousHelper target (first :: range) rest

        EQ ->
            Just range

        GT ->
            Nothing


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

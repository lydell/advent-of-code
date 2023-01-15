module Day2 exposing (..)

import Array exposing (Array)
import Day2Input exposing (puzzleInput)
import Html exposing (Html)
import LineParser
import Regex exposing (Regex)


type alias Password =
    { password : String
    , char : Char
    , min : Int
    , max : Int
    }


lineRegex : Regex
lineRegex =
    Regex.fromString "^(\\d+)-(\\d+) ([a-z]): ([a-z]+)$"
        |> Maybe.withDefault Regex.never


parse : String -> Result String (List Password)
parse =
    LineParser.parse
        (Regex.findAtMost 1 lineRegex
            >> List.head
            >> Result.fromMaybe "Didn’t match the regex :("
            >> Result.andThen
                (\match ->
                    case match.submatches of
                        [ Just min, Just max, Just char, Just password ] ->
                            Result.map3 (Password password)
                                (String.uncons char |> Maybe.map Tuple.first |> Result.fromMaybe "String.uncons failed")
                                (String.toInt min |> Result.fromMaybe "min to integer failed")
                                (String.toInt max |> Result.fromMaybe "max to integer failed")

                        _ ->
                            Err "Regex submatches not as expected :("
                )
        )


solution1 : String -> Result String Int
solution1 =
    parse >> Result.map (List.filter isValid1 >> List.length)


solution2 : String -> Result String Int
solution2 =
    parse >> Result.map (List.filter isValid2 >> List.length)


isValid1 : Password -> Bool
isValid1 password =
    let
        numOfChar =
            String.filter ((==) password.char) password.password
                |> String.length
    in
    numOfChar >= password.min && numOfChar <= password.max


isValid2 : Password -> Bool
isValid2 password =
    let
        array =
            password.password
                |> String.toList
                |> Array.fromList

        isCharAt index =
            case Array.get (index - 1) array of
                Just char ->
                    char == password.char

                Nothing ->
                    False
    in
    xor (isCharAt password.min) (isCharAt password.max)


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

module Day2 exposing (..)

import Array exposing (Array)
import Day2Input exposing (puzzleInput)
import Html exposing (Html)
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


parse : String -> List Password
parse =
    String.trim
        >> String.split "\n"
        >> List.filterMap
            (Regex.findAtMost 1 lineRegex
                >> List.head
                >> Maybe.andThen
                    (\match ->
                        case match.submatches of
                            [ Just min, Just max, Just char, Just password ] ->
                                Maybe.map3 (Password password)
                                    (String.uncons char |> Maybe.map Tuple.first)
                                    (String.toInt min)
                                    (String.toInt max)

                            _ ->
                                Nothing
                    )
            )


solution1 : String -> Int
solution1 =
    parse >> List.filter isValid1 >> List.length


solution2 : String -> Int
solution2 =
    parse >> List.filter isValid2 >> List.length


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
        [ Html.div [] [ Html.text (String.fromInt (solution1 puzzleInput)) ]
        , Html.div [] [ Html.text (String.fromInt (solution2 puzzleInput)) ]
        ]

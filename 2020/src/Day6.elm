module Day6 exposing (..)

import Day6Input exposing (puzzleInput)
import Html exposing (Html)
import Regex exposing (Regex)
import Set exposing (Set)


solution1 : String -> Int
solution1 =
    String.trim
        >> String.split "\n\n"
        >> List.map
            (String.filter Char.isAlpha
                >> String.toList
                >> Set.fromList
                >> Set.size
            )
        >> List.sum


allLetters : Set Char
allLetters =
    List.range (Char.toCode 'a') (Char.toCode 'z')
        |> List.map Char.fromCode
        |> Set.fromList


solution2 : String -> Int
solution2 =
    String.trim
        >> String.split "\n\n"
        >> List.map
            (String.lines
                >> List.map (String.toList >> Set.fromList)
                >> List.foldl Set.intersect allLetters
                >> Set.size
            )
        >> List.sum


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
        , showResult (solution2 puzzleInput)
        ]


showResult : Int -> Html msg
showResult result =
    Html.output []
        [ Html.text (String.fromInt result) ]

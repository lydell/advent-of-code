module Day4 exposing (..)

import Day4Input exposing (puzzleInput)
import Html exposing (Html)
import Set exposing (Set)


requiredFields : Set String
requiredFields =
    Set.fromList
        [ "byr"
        , "iyr"
        , "eyr"
        , "hgt"
        , "hcl"
        , "ecl"
        , "pid"
        ]


solution1 : String -> Int
solution1 =
    String.trim
        >> String.split "\n\n"
        >> List.filter
            (String.words
                >> List.filterMap (String.split ":" >> List.head)
                >> Set.fromList
                >> Set.diff requiredFields
                >> Set.isEmpty
            )
        >> List.length


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput |> Ok)
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

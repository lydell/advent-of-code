module Day4 exposing (..)

import Day4Input exposing (puzzleInput)
import Html exposing (Html)
import Regex exposing (Regex)
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


solution2 : String -> Int
solution2 =
    String.trim
        >> String.split "\n\n"
        >> List.filter isValid
        >> List.length


isValid : String -> Bool
isValid input =
    List.all (\regex -> Regex.contains regex input) regexes


regexes : List Regex
regexes =
    [ "byr:(19[2-9]\\d|19[4-9]\\d|200[0-2])"
    , "iyr:(201\\d|2020)"
    , "eyr:(202\\d|2030)"
    , "hgt:((1[5-8]\\d|19[0-3])cm|(59|6\\d|7[0-6])in)"
    , "hcl:#[\\da-f]{6}"
    , "ecl:(amb|blu|brn|gry|grn|hzl|oth)"
    , "pid:\\d{9}"
    ]
        |> List.map
            (\string ->
                "(^|\\s)"
                    ++ string
                    ++ "(\\s|$)"
                    |> Regex.fromString
                    |> Maybe.withDefault Regex.never
            )


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

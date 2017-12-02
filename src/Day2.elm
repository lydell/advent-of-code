module Day2 exposing (checksum, dividesum, output, parse)

import Day2Input exposing (spreadsheet)
import List.Extra


output : ( String, String )
output =
    ( spreadsheet |> parse |> checksum |> toString
    , spreadsheet |> parse |> dividesum |> toString
    )


checksum : List (List Int) -> Int
checksum spreadsheet =
    spreadsheet
        |> List.map minMaxDifference
        |> List.sum


minMaxDifference : List Int -> Int
minMaxDifference list =
    let
        minimum =
            list |> List.minimum |> Maybe.withDefault 0

        maximum =
            list |> List.maximum |> Maybe.withDefault 0
    in
    maximum - minimum


dividesum : List (List Int) -> Int
dividesum spreadsheet =
    spreadsheet
        |> List.filterMap rowDivide
        |> List.sum


rowDivide : List Int -> Maybe Int
rowDivide list =
    list
        |> List.filterMap (findDividingPair list)
        |> List.head
        |> Maybe.map (uncurry (//))


findDividingPair : List Int -> Int -> Maybe ( Int, Int )
findDividingPair list numerator =
    list
        |> List.Extra.find
            (\denominator ->
                denominator /= numerator && rem numerator denominator == 0
            )
        |> Maybe.map ((,) numerator)


parse : String -> List (List Int)
parse string =
    string
        |> String.lines
        |> List.map parseLine


parseLine : String -> List Int
parseLine string =
    string
        |> String.words
        |> List.filterMap (String.toInt >> Result.toMaybe)

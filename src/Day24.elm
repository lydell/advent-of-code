module Day24 exposing (..)

import Day24Input exposing (puzzleInput)
import Hexagons.Hex as Hex exposing (Direction, Hex)
import Hexagons.Map
import Html exposing (Html)
import LineParser
import Regex exposing (Regex)
import Set


regex : Regex
regex =
    Regex.fromString "e|se|sw|w|nw|ne|."
        |> Maybe.withDefault Regex.never


parse : String -> Result String (List (List Direction))
parse =
    LineParser.parse
        (Regex.find regex
            >> List.map .match
            >> LineParser.parseGeneral "Direction"
                identity
                (\direction ->
                    case direction of
                        "e" ->
                            Ok Hex.E

                        "se" ->
                            Ok Hex.SE

                        "sw" ->
                            Ok Hex.SW

                        "w" ->
                            Ok Hex.W

                        "nw" ->
                            Ok Hex.NW

                        "ne" ->
                            Ok Hex.NE

                        _ ->
                            Err ("Unknown direction: " ++ direction)
                )
        )


solution1 : String -> Result String Int
solution1 =
    parse >> Result.map solve1


solve1 : List (List Direction) -> Int
solve1 =
    List.foldl
        (\directions blacks ->
            let
                next =
                    List.foldl
                        (\direction hex ->
                            Hex.neighbor hex direction
                        )
                        (Hex.intFactory ( 0, 0 ))
                        directions
                        |> Hexagons.Map.hashHex
            in
            if Set.member next blacks then
                Set.remove next blacks

            else
                Set.insert next blacks
        )
        Set.empty
        >> Set.size


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

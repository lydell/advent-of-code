module Day10 exposing (..)

import Day10Input exposing (puzzleInput)
import Html exposing (Html)
import LineParser
import Set


parse : String -> Result String (List Int)
parse =
    LineParser.parse (String.toInt >> Result.fromMaybe "Not a number.")


solution1 : String -> Result String Int
solution1 =
    parse >> Result.andThen solve1


solve1 : List Int -> Result String Int
solve1 adapters =
    let
        chargingOutlet =
            0

        deviceAdapter =
            case List.maximum adapters of
                Just jolt ->
                    [ jolt + 3 ]

                Nothing ->
                    []

        sorted =
            List.sort adapters

        diffs =
            List.map2 (-)
                (sorted ++ deviceAdapter)
                (chargingOutlet :: sorted)

        ones =
            diffs |> List.filter ((==) 1) |> List.length

        threes =
            diffs |> List.filter ((==) 3) |> List.length

        set =
            diffs |> List.filter ((/=) 2) |> Set.fromList
    in
    if set == Set.fromList [ 1, 3 ] then
        Ok (ones * threes)

    else
        Err
            ("Expected only 1, 2, 3 as differences, but got: "
                ++ (set |> Set.toList |> List.map String.fromInt |> String.join ", ")
            )


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

module Day16 exposing (..)

import Day16Input exposing (puzzleInput)
import Dict exposing (Dict)
import Html exposing (Html)
import LineParser
import Set exposing (Set)


type alias Input =
    { rules : Dict String (Set Int)
    , yourTicket : List Int
    , nearbyTickets : List (List Int)
    }


parse : String -> Result String Input
parse input =
    let
        parts =
            input |> String.trim |> String.split "\n\n"
    in
    case parts of
        [ first, second, third ] ->
            Result.map3 Input
                (parseRules first)
                (parseTicket (dropFirstLine second))
                (LineParser.parse parseTicket (dropFirstLine third))

        _ ->
            Err ("Expected 3 parts but got " ++ String.fromInt (List.length parts))


dropFirstLine : String -> String
dropFirstLine =
    String.lines
        >> List.drop 1
        >> String.join "\n"


parseRules : String -> Result String (Dict String (Set Int))
parseRules =
    LineParser.parse
        (\line ->
            case String.split ": " line of
                [ before, after ] ->
                    String.split " or " after
                        |> LineParser.parseGeneral "Range"
                            identity
                            (\range ->
                                case String.split "-" range of
                                    [ lower, upper ] ->
                                        Result.map2 List.range
                                            (String.toInt lower |> Result.fromMaybe "Lower is not a number.")
                                            (String.toInt upper |> Result.fromMaybe "Upper is not a number.")

                                    parts ->
                                        Err ("Expected 2 dash-separated parts but got " ++ String.fromInt (List.length parts))
                            )
                        |> Result.map
                            (List.concat
                                >> Set.fromList
                                >> Tuple.pair before
                            )

                parts ->
                    Err ("Expected 2 colon-separated parts but got " ++ String.fromInt (List.length parts))
        )
        >> Result.map Dict.fromList


parseTicket : String -> Result String (List Int)
parseTicket =
    String.split ","
        >> LineParser.parseGeneral "Number"
            identity
            (String.toInt >> Result.fromMaybe "Not a number.")


solution1 : String -> Result String Int
solution1 =
    parse >> Result.map solve1


solve1 : Input -> Int
solve1 input =
    let
        fullRange : Set Int
        fullRange =
            input.rules
                |> Dict.values
                |> List.foldl Set.union Set.empty
    in
    input.nearbyTickets
        |> List.concatMap
            (List.filterMap
                (\number ->
                    if Set.member number fullRange then
                        Nothing

                    else
                        Just number
                )
            )
        |> List.sum


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

module Day13 exposing (..)

import Day13Input exposing (input)
import Firewall exposing (findUncaughtDelay, fromList, getSeverity, iterate)


output : () -> ( String, String )
output () =
    ( input |> runThrough |> toString
    , input |> runThroughUncaught |> toString
    )


runThrough : String -> Int
runThrough =
    parse >> fromList >> iterate >> getSeverity


runThroughUncaught : String -> Int
runThroughUncaught =
    parse >> fromList >> findUncaughtDelay


parse : String -> List ( Int, Int )
parse string =
    string
        |> String.lines
        |> List.filterMap parseLine


parseLine : String -> Maybe ( Int, Int )
parseLine string =
    let
        words =
            string
                |> String.filter ((/=) ',')
                |> String.words
    in
    case String.split ": " string of
        [ depthString, rangeString ] ->
            case ( String.toInt depthString, String.toInt rangeString ) of
                ( Ok depth, Ok range ) ->
                    Just ( depth, range )

                _ ->
                    Nothing

        _ ->
            Nothing

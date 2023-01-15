module Day10 exposing (..)

import Day10Input exposing (input)
import KnotHash


output : () -> ( String, String )
output () =
    ( parse input |> check 256 |> toString
    , KnotHash.hash256 input
    )


check : Int -> List Int -> Maybe Int
check n lengths =
    KnotHash.fromList (List.range 0 (n - 1))
        |> KnotHash.stepThrough lengths
        |> KnotHash.check


parse : String -> List Int
parse string =
    string
        |> String.split ","
        |> List.filterMap (String.toInt >> Result.toMaybe)

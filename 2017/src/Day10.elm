module Day10 exposing (..)

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


input : String
input =
    "206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3"

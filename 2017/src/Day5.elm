module Day5 exposing (..)

import Day5Input exposing (input)
import JumpList exposing (JumpList, JumpListInspection)


output : () -> ( String, String )
output () =
    ( input |> jumpThrough1 |> .numJumps |> toString
    , input |> jumpThrough2 |> .numJumps |> toString
    )


jumpThrough1 : String -> JumpListInspection
jumpThrough1 =
    jumpThrough identity


jumpThrough2 : String -> JumpListInspection
jumpThrough2 =
    jumpThrough (JumpList.withTransform part2Transform)


part2Transform : Int -> Int
part2Transform number =
    if number >= 3 then
        number - 1

    else
        number + 1


jumpThrough : (JumpList -> JumpList) -> String -> JumpListInspection
jumpThrough f string =
    string
        |> parse
        |> JumpList.fromList
        |> f
        |> JumpList.iterate
        |> JumpList.inspect


parse : String -> List Int
parse string =
    string
        |> String.lines
        |> List.filterMap (String.toInt >> Result.toMaybe)

module Day17 exposing (..)

import Day17Input exposing (input)
import List.Extra


output : () -> ( String, String )
output () =
    ( stepN 2017 input |> getFollowingValue |> toString
    , ""
    )


type alias Buffer =
    { list : List Int
    , stepSize : Int
    , pos : Int
    , nextValue : Int
    }


empty : Int -> Buffer
empty stepSize =
    { list = [], stepSize = stepSize, pos = 0, nextValue = 0 }


step : Buffer -> Buffer
step { list, stepSize, pos, nextValue } =
    let
        length =
            List.length list

        newPos =
            if length == 0 then
                0

            else
                rem (pos + stepSize) length + 1

        ( before, after ) =
            List.Extra.splitAt newPos list

        newList =
            before ++ [ nextValue ] ++ after
    in
    { list = newList
    , stepSize = stepSize
    , pos = newPos
    , nextValue = nextValue + 1
    }


stepN : Int -> Int -> Buffer
stepN n stepSize =
    List.repeat (n + 1) ()
        |> List.foldl (always step) (empty stepSize)


getFollowingValue : Buffer -> Maybe Int
getFollowingValue { list, pos } =
    case list of
        [] ->
            Nothing

        _ ->
            List.Extra.getAt (rem (pos + 1) (List.length list)) list

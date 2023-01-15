module Day17 exposing (..)

import List.Extra


output : () -> ( String, String )
output () =
    ( stepN 2017 input |> getFollowingValue |> toString
    , getNumberAfterZeroN (truncate 5.0e7) input |> toString
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


getNumberAfterZeroN : Int -> Int -> Maybe Int
getNumberAfterZeroN n stepSize =
    getNumberAfterZeroNHelper n stepSize 0 0 Nothing


getNumberAfterZeroNHelper : Int -> Int -> Int -> Int -> Maybe Int -> Maybe Int
getNumberAfterZeroNHelper n stepSize pos nextValue numberAfterZero =
    if nextValue <= n then
        let
            newPos =
                if nextValue == 0 then
                    0
                else
                    rem (pos + stepSize) nextValue + 1

            newNumberAfterZero =
                if newPos == 1 then
                    Just nextValue
                else
                    numberAfterZero
        in
        getNumberAfterZeroNHelper n stepSize newPos (nextValue + 1) newNumberAfterZero
    else
        numberAfterZero


input : Int
input =
    329

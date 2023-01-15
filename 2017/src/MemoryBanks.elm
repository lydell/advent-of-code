module MemoryBanks exposing (MemoryBanks, MemoryBanksResult, fromList, iterate)

import Dict exposing (Dict)
import List.Extra


type MemoryBanks
    = MemoryBanks
        { list : List Int
        , numCycles : Int
        }


type alias MemoryBanksResult =
    { list : List Int
    , numCycles : Int
    , loopLength : Int
    }


fromList : List Int -> MemoryBanks
fromList list =
    MemoryBanks
        { list = list
        , numCycles = 0
        }


next : MemoryBanks -> MemoryBanks
next (MemoryBanks { list, numCycles }) =
    let
        newList =
            list
                |> List.indexedMap (,)
                |> List.Extra.maximumBy (\( index, number ) -> ( number, -index ))
                |> Maybe.map (updateList list)
                |> Maybe.withDefault list
    in
    MemoryBanks
        { list = newList
        , numCycles = numCycles + 1
        }


updateList : List Int -> ( Int, Int ) -> List Int
updateList list ( largestIndex, largestNumber ) =
    let
        length =
            List.length list

        minIncrement =
            largestNumber // length

        extra =
            rem largestNumber length
    in
    list
        |> List.indexedMap
            (\index number ->
                let
                    increment =
                        if ((index - largestIndex - 1) % length) < extra then
                            minIncrement + 1
                        else
                            minIncrement
                in
                if index == largestIndex then
                    increment
                else
                    number + increment
            )


iterate : MemoryBanks -> MemoryBanksResult
iterate memoryBanks =
    let
        (MemoryBanks { list, numCycles }) =
            memoryBanks

        ( seen, newMemoryBanks ) =
            iterateHelper ( Dict.singleton list numCycles, memoryBanks )

        (MemoryBanks details) =
            newMemoryBanks

        lastNumCycles =
            Dict.get details.list seen
                |> Maybe.withDefault details.numCycles

        loopLength =
            details.numCycles - lastNumCycles
    in
    { list = details.list
    , numCycles = details.numCycles
    , loopLength = loopLength
    }


iterateHelper :
    ( Dict (List Int) Int, MemoryBanks )
    -> ( Dict (List Int) Int, MemoryBanks )
iterateHelper ( seen, memoryBanks ) =
    let
        nextMemoryBanks =
            next memoryBanks

        (MemoryBanks { list, numCycles }) =
            nextMemoryBanks
    in
    if Dict.member list seen then
        ( seen, nextMemoryBanks )
    else
        iterateHelper ( Dict.insert list numCycles seen, nextMemoryBanks )

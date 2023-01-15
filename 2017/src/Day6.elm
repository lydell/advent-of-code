module Day6 exposing (..)

import MemoryBanks exposing (MemoryBanksResult)


output : () -> ( String, String )
output () =
    ( input |> cycleThrough |> .numCycles |> toString
    , input |> cycleThrough |> .loopLength |> toString
    )


cycleThrough : List Int -> MemoryBanksResult
cycleThrough =
    MemoryBanks.fromList >> MemoryBanks.iterate


input : List Int
input =
    [ 10, 3, 15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6 ]

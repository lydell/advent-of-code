module Day6 exposing (cycleThrough, output)

import Day6Input exposing (input)
import MemoryBanks exposing (MemoryBanksResult)


output : ( String, String )
output =
    ( input |> cycleThrough |> .numCycles |> toString
    , input |> cycleThrough |> .loopLength |> toString
    )


cycleThrough : List Int -> MemoryBanksResult
cycleThrough =
    MemoryBanks.fromList >> MemoryBanks.iterate

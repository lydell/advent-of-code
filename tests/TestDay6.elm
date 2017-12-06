module TestDay6 exposing (..)

import Day6 exposing (cycleThrough)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Day5"
        [ describe "cycleThrough"
            [ test "it works" <|
                \_ ->
                    cycleThrough [ 0, 2, 7, 0 ]
                        |> Expect.equal
                            { list = [ 2, 4, 1, 2 ]
                            , numCycles = 5
                            , loopLength = 4
                            }
            ]
        ]

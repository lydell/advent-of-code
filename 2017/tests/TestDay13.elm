module TestDay13 exposing (..)

import Day13 exposing (findUncaughtDelay, parse, totalSeverity)
import Expect exposing (Expectation)
import Test exposing (..)


input : String
input =
    """0: 3
1: 2
4: 4
6: 4"""


suite : Test
suite =
    describe "Day13"
        [ describe "totalSeverity"
            [ test "it works" <|
                \_ ->
                    totalSeverity (parse input)
                        |> Expect.equal 24
            ]
        , describe "findUncaughtDelay"
            [ test "it works" <|
                \_ ->
                    findUncaughtDelay (parse input)
                        |> Expect.equal 10
            ]
        ]

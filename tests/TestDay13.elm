module TestDay13 exposing (..)

import Day13 exposing (runThrough, runThroughUncaught)
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
        [ describe "runThrough"
            [ test "it works" <|
                \_ ->
                    runThrough input
                        |> Expect.equal 24
            ]
        , describe "runThroughUncaught"
            [ test "it works" <|
                \_ ->
                    runThroughUncaught input
                        |> Expect.equal 10
            ]
        ]

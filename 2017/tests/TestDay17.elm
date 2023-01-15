module TestDay17 exposing (..)

import Day17 exposing (getFollowingValue, getNumberAfterZeroN, stepN)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Day17"
        [ describe "stepN and getFollowingValue"
            [ test "they work" <|
                \_ ->
                    getFollowingValue (stepN 2017 3)
                        |> Expect.equal (Just 638)
            ]
        , describe "getNumberAfterZeroN"
            [ test "they work" <|
                \_ ->
                    getNumberAfterZeroN 2017 3
                        |> Expect.equal (Just 1226)
            ]
        ]

module TestDay17 exposing (..)

import Day17 exposing (getFollowingValue, stepN)
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
        ]

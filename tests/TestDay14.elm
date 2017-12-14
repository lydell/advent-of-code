module TestDay14 exposing (..)

import Day14 exposing (countY, makeGrid)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Day14"
        [ describe "countY"
            [ test "it works" <|
                \_ ->
                    let
                        grid =
                            makeGrid "flqrgnkx"

                        grid8x8 =
                            grid
                                |> List.take 8
                                |> List.map (List.take 8)
                    in
                    countY grid8x8
                        |> Expect.equal 29
            ]
        ]

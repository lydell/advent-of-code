module TestDay14 exposing (..)

import Day14 exposing (Binary, countClusters, countY, makeGrid)
import Expect exposing (Expectation)
import Test exposing (..)


grid : List (List Binary)
grid =
    makeGrid "flqrgnkx"


suite : Test
suite =
    describe "Day14"
        [ describe "countY"
            [ test "it works" <|
                \_ ->
                    let
                        grid8x8 =
                            grid
                                |> List.take 8
                                |> List.map (List.take 8)
                    in
                    countY grid8x8
                        |> Expect.equal 29
            ]
        , describe "countClusters"
            [ test "it works" <|
                \_ ->
                    countClusters grid
                        |> Expect.equal (Just 1242)
            ]
        ]

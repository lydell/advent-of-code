module TestDay5 exposing (..)

import Day5 exposing (jumpThrough1, jumpThrough2)
import Expect exposing (Expectation)
import Test exposing (..)


input : String
input =
    """0
3
0
1
-3"""


suite : Test
suite =
    describe "Day5"
        [ describe "jumpThrough1"
            [ test "it works" <|
                \_ ->
                    jumpThrough1 input
                        |> Expect.equal
                            { list = [ 2, 5, 0, 1, -2 ]
                            , index = 5
                            , numJumps = 5
                            }
            ]
        , describe "jumpThrough2"
            [ test "it works" <|
                \_ ->
                    jumpThrough2 input
                        |> Expect.equal
                            { list = [ 2, 3, 2, 3, -1 ]
                            , index = 5
                            , numJumps = 10
                            }
            ]
        ]

module TestDay19 exposing (..)

import Day19 exposing (goThrough, parse)
import Expect exposing (Expectation)
import Test exposing (..)


input1 : String
input1 =
    """     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+"""


input2 : String
input2 =
    """|
A  +B-+
++ +-+| C
 |+-+|++|
 ++ ++ ++"""


suite : Test
suite =
    describe "Day19"
        [ describe "goThrough"
            [ test "it works" <|
                \_ ->
                    goThrough (parse input1)
                        |> Expect.equal ( 38, [ 'A', 'B', 'C', 'D', 'E', 'F' ] )
            , test "complicated case" <|
                \_ ->
                    goThrough (parse input2)
                        |> Expect.equal ( 27, [ 'A', 'B', 'C' ] )
            ]
        ]

module TestDay18 exposing (..)

import Day18 exposing (firstRecovered, parse)
import Expect exposing (Expectation)
import Test exposing (..)


input : String
input =
    """set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"""


suite : Test
suite =
    describe "Day18"
        [ describe "firstRecovered"
            [ test "it works" <|
                \_ ->
                    firstRecovered (parse input)
                        |> Expect.equal (Just 4)
            ]
        ]

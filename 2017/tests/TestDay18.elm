module TestDay18 exposing (..)

import Day18 exposing (duet, firstReceived, parse)
import Expect exposing (Expectation)
import Test exposing (..)


input1 : String
input1 =
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


input2 : String
input2 =
    """snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d"""


input3 : String
input3 =
    """set s 1
set t 2
snd s
jgz p 5

rcv a
rcv a
add t 2
rcv a

mul t -1
mod t 0
snd t"""


suite : Test
suite =
    describe "Day18"
        [ describe "firstReceived"
            [ test "it works" <|
                \_ ->
                    firstReceived (parse input1)
                        |> Expect.equal (Just 4)
            ]
        , describe "duet"
            [ test "provided example" <|
                \_ ->
                    let
                        ( program0, program1 ) =
                            input2 |> parse |> duet
                    in
                    ( program0.numSent, program1.numSent )
                        |> Expect.equal ( 3, 3 )
            , test "input3" <|
                \_ ->
                    let
                        ( program0, program1 ) =
                            input3 |> parse |> duet
                    in
                    ( program0.numSent, program1.numSent )
                        |> Expect.equal ( 1, 2 )
            ]
        ]

module TestDay20 exposing (..)

import Day20 exposing (findClosest, findNumSurvivors, parse)
import Expect exposing (Expectation)
import Test exposing (..)


input1 : String
input1 =
    """p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"""


input2 : String
input2 =
    """p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"""


suite : Test
suite =
    describe "Day20"
        [ describe "findClosest"
            [ test "it works" <|
                \_ ->
                    findClosest 3 (parse input1)
                        |> Expect.equalLists [ 0 ]
            ]
        , describe "findNumSurvivors"
            [ test "it works" <|
                \_ ->
                    findNumSurvivors 3 (parse input2)
                        |> Expect.equal 1
            ]
        ]

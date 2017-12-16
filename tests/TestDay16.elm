module TestDay16 exposing (..)

import Day16 exposing (evaluate, parse)
import Expect exposing (Expectation)
import Test exposing (..)


input : String
input =
    "s1,x3/4,pe/b"


dancers5 : List String
dancers5 =
    [ "a", "b", "c", "d", "e" ]


suite : Test
suite =
    describe "Day16"
        [ describe "evaluate"
            [ test "it works" <|
                \_ ->
                    evaluate (parse input) dancers5
                        |> Expect.equalLists
                            [ "b", "a", "e", "d", "c" ]
            ]
        ]

module TestDay15 exposing (..)

import Day15 exposing (generateN, generatorA, generatorB, judge, judgeN, matchesNLowestBits)
import Expect exposing (Expectation)
import Test exposing (..)


startA : Int
startA =
    65


startB : Int
startB =
    8921


suite : Test
suite =
    describe "Day15"
        [ describe "generateN"
            [ test "generatorA" <|
                \_ ->
                    generateN 5 startA generatorA
                        |> Expect.equalLists
                            [ 1092455
                            , 1181022009
                            , 245556042
                            , 1744312007
                            , 1352636452
                            ]
            , test "generatorB" <|
                \_ ->
                    generateN 5 startB generatorB
                        |> Expect.equalLists
                            [ 430625591
                            , 1233683848
                            , 1431495498
                            , 137874439
                            , 285222916
                            ]
            ]
        , describe "matchesNLowestBits"
            [ test "16, no match" <|
                \_ ->
                    matchesNLowestBits 16 1092455 430625591
                        |> Expect.equal False
            , test "16, match" <|
                \_ ->
                    matchesNLowestBits 16 245556042 1431495498
                        |> Expect.equal True
            ]
        , describe "judgeN"
            [ test "it works" <|
                \_ ->
                    judgeN 5 16 ( startA, startB ) ( generatorA, generatorB )
                        |> Expect.equal 1
            ]
        , describe "judge"
            [ test "it works" <|
                \_ ->
                    judge ( startA, startB ) ( generatorA, generatorB )
                        |> Expect.equal 588
            ]
        ]

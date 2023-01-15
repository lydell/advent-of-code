module TestDay15 exposing (..)

import Day15 exposing (Generator, generateN, generatorA, generatorAFiltered, generatorB, generatorBFiltered, judge, judgeFiltered, judgeN, matchesNLowestBits)
import Expect exposing (Expectation)
import Test exposing (..)


testA : Generator -> Generator
testA generator =
    { generator | value = 65 }


testB : Generator -> Generator
testB generator =
    { generator | value = 8921 }


suite : Test
suite =
    describe "Day15"
        [ describe "generateN"
            [ test "generatorA" <|
                \_ ->
                    generateN 5 (testA generatorA)
                        |> Expect.equalLists
                            [ 1092455
                            , 1181022009
                            , 245556042
                            , 1744312007
                            , 1352636452
                            ]
            , test "generatorB" <|
                \_ ->
                    generateN 5 (testB generatorB)
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
                    judgeN 5 16 ( testA generatorA, testB generatorB )
                        |> Expect.equal 1
            ]
        , describe "judge"
            [ test "it works" <|
                \_ ->
                    judge ( testA generatorA, testB generatorB )
                        |> Expect.equal 588
            ]
        , describe "judgeFiltered"
            [ test "it works" <|
                \_ ->
                    judgeFiltered ( testA generatorAFiltered, testB generatorBFiltered )
                        |> Expect.equal 309
            ]
        ]

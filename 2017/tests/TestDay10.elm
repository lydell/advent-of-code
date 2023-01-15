module TestDay10 exposing (..)

import Day10
import Expect exposing (Expectation)
import KnotHash exposing (KnotHash, check, dense, fromList, hash256, inspect, next, stepThrough, stepThroughN, stringToCharCodes, toHex)
import Test exposing (..)


new5 : KnotHash
new5 =
    fromList (List.range 0 4)


lengths : List Int
lengths =
    [ 3, 4, 1, 5 ]


hash256Tests : List ( String, String )
hash256Tests =
    [ ( "", "a2582a3a0e66e6e86e3812dcb672a272" )
    , ( "AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd" )
    , ( "1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d" )
    , ( "1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e" )
    ]


testHash256 : ( String, String ) -> Test
testHash256 ( string, expected ) =
    test
        (if string == "" then
            "(empty string)"
         else
            string
        )
    <|
        \_ ->
            hash256 string
                |> Expect.equal expected


suite : Test
suite =
    describe "Day10"
        [ describe "KnotHash"
            [ test "next" <|
                \_ ->
                    inspect (next 3 new5)
                        |> Expect.equal
                            { list = [ 2, 1, 0, 3, 4 ]
                            , pos = 3
                            , skip = 1
                            }
            , test "stepThrough" <|
                \_ ->
                    inspect (stepThrough lengths new5)
                        |> Expect.equal
                            { list = [ 3, 4, 2, 1, 0 ]
                            , pos = 4
                            , skip = 4
                            }
            , test "stepThroughN" <|
                \_ ->
                    inspect (stepThroughN 2 lengths new5)
                        |> Expect.equal
                            { list = [ 2, 1, 4, 3, 0 ]
                            , pos = 4
                            , skip = 8
                            }
            , test "check" <|
                \_ ->
                    check (stepThrough lengths new5)
                        |> Expect.equal (Just 12)
            , test "dense" <|
                \_ ->
                    dense 16 (fromList [ 65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22 ])
                        |> Expect.equalLists [ 64 ]
            , test "toHex" <|
                \_ ->
                    toHex [ 64, 7, 255 ]
                        |> Expect.equal "4007ff"
            , test "stringToCharCodes" <|
                \_ ->
                    stringToCharCodes "1,2,3"
                        |> Expect.equalLists [ 49, 44, 50, 44, 51 ]
            , describe "hash256" (List.map testHash256 hash256Tests)
            ]
        , test "check" <|
            \_ ->
                Day10.check 5 lengths
                    |> Expect.equal (Just 12)
        ]

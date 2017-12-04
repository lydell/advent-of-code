module TestDay3 exposing (..)

import AddingSpiral exposing (toListOfLists)
import Day3 exposing (distance, firstLargerNumber, spiralLargerThan, straightCos)
import Expect exposing (Expectation)
import Fuzz
import Test exposing (..)


{-
   37  36  35  34  33  32  31
   38  17  16  15  14  13  30
   39  18   5   4   3  12  29
   40  19   6   1   2  11  28
   41  20   7   8   9  10  27
   42  21  22  23  24  25  26
   43  44  45  46  47  48  49
-}


tests1 : List ( Int, Int )
tests1 =
    [ -- 1
      ( 1, 0 )

    -- 2
    , ( 2, 1 )
    , ( 3, 2 )
    , ( 4, 1 )
    , ( 5, 2 )
    , ( 6, 1 )
    , ( 7, 2 )
    , ( 8, 1 )
    , ( 9, 2 )

    -- 3
    , ( 10, 3 )
    , ( 11, 2 )
    , ( 12, 3 )
    , ( 13, 4 )
    , ( 14, 3 )
    , ( 15, 2 )
    , ( 16, 3 )
    , ( 17, 4 )
    , ( 18, 3 )
    , ( 19, 2 )
    , ( 20, 3 )
    , ( 21, 4 )
    , ( 22, 3 )
    , ( 23, 2 )
    , ( 24, 3 )
    , ( 25, 4 )

    -- 4
    , ( 26, 5 )
    , ( 27, 4 )
    , ( 28, 3 )
    , ( 29, 4 )
    , ( 30, 5 )
    , ( 31, 6 )
    , ( 32, 5 )
    , ( 33, 4 )
    , ( 34, 3 )
    , ( 35, 4 )
    , ( 36, 5 )
    , ( 37, 6 )
    , ( 38, 5 )
    , ( 39, 4 )
    , ( 40, 3 )
    , ( 41, 4 )
    , ( 42, 5 )
    , ( 43, 6 )
    , ( 44, 5 )
    , ( 45, 4 )
    , ( 46, 3 )
    , ( 47, 4 )
    , ( 48, 5 )
    , ( 49, 6 )
    ]


suite : Test
suite =
    describe "Day3"
        [ describe "distance" <|
            runTests distance tests1
                ++ [ fuzz (Fuzz.intRange 1 94906266)
                        "the bottom-right corner is always a square and has an easy-to-calculate distance"
                     <|
                        \n ->
                            distance (n ^ 2)
                                |> Expect.equal (n - 1)
                   ]
        , describe "straightCos" <|
            [ fuzz (Fuzz.floatRange -(2 ^ 31 - 1) (2 ^ 31 - 1))
                "its output is always between -1 and 1"
              <|
                \float ->
                    straightCos float
                        |> Expect.all
                            [ Expect.atLeast -1
                            , Expect.atMost 1
                            ]
            ]
                ++ ([ ( 0, 1 ), ( 0.25, 0 ), ( 0.5, -1 ), ( 0.75, 0 ), ( 1, 1 ), ( 1.25, 0 ) ]
                        |> List.map
                            (\( input, expected ) ->
                                test (toString input) <|
                                    \_ ->
                                        straightCos input
                                            |> Expect.equal expected
                            )
                   )
        , describe "adding spiral"
            [ test "first three rings" <|
                \_ ->
                    toListOfLists (spiralLargerThan 930)
                        |> Expect.equalLists
                            [ [ 147, 142, 133, 122, 59 ]
                            , [ 304, 5, 4, 2, 57 ]
                            , [ 330, 10, 1, 1, 54 ]
                            , [ 351, 11, 23, 25, 26 ]
                            , [ 362, 747, 806, 880, 931 ]
                            ]
            , test "larger than 1" <|
                \_ ->
                    firstLargerNumber 1
                        |> Expect.equal 2
            , test "larger than 80" <|
                \_ ->
                    firstLargerNumber 80
                        |> Expect.equal 122
            ]
        ]


runTests : (Int -> Int) -> List ( Int, Int ) -> List Test
runTests f tests =
    List.map (runTest f) tests


runTest : (Int -> Int) -> ( Int, Int ) -> Test
runTest f ( input, expected ) =
    test (toString input) <|
        \_ ->
            f input
                |> Expect.equal expected

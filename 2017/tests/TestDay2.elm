module TestDay2 exposing (..)

import Day2 exposing (checksum, dividesum, parse)
import Expect exposing (Expectation)
import Test exposing (..)


spreadsheet1 : List (List Int)
spreadsheet1 =
    [ [ 5, 1, 9, 5 ]
    , [ 7, 5, 3 ]
    , [ 2, 4, 6, 8 ]
    ]


spreadsheet2 : List (List Int)
spreadsheet2 =
    [ [ 5, 9, 2, 8 ]
    , [ 9, 4, 7, 3 ]
    , [ 3, 8, 6, 5 ]
    ]


spreadsheetString : String
spreadsheetString =
    """1 2 34
5  6\t789
      \t
  0"""


spreadsheetStringParsed : List (List Int)
spreadsheetStringParsed =
    [ [ 1, 2, 34 ]
    , [ 5, 6, 789 ]
    , []
    , [ 0 ]
    ]


suite : Test
suite =
    describe "Day2"
        [ describe "checksum"
            [ test "basic" <|
                \_ ->
                    checksum spreadsheet1
                        |> Expect.equal 18
            ]
        , describe "dividesum"
            [ test "basic" <|
                \_ ->
                    dividesum spreadsheet2
                        |> Expect.equal 9
            ]
        , describe "parse"
            [ test "basic" <|
                \_ ->
                    parse spreadsheetString
                        |> Expect.equal spreadsheetStringParsed
            ]
        ]

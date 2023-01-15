module TestDay4 exposing (..)

import Day4 exposing (numValid, validatePassPhrase1, validatePassPhrase2)
import Expect exposing (Expectation)
import Test exposing (..)


passPhrases1 : String
passPhrases1 =
    """aa bb cc dd ee
aa bb cc dd aa
aa bb cc dd aaa"""


passPhrases2 : String
passPhrases2 =
    """abcde fghij
abcde xyz ecdab
a ab abc abd abf abj
iiii oiii ooii oooi oooo
oiii ioii iioi iiio"""


suite : Test
suite =
    describe "Day4"
        [ describe "validatePassPhrase1"
            [ test "valid" <|
                \_ ->
                    validatePassPhrase1 "aa bb cc dd ee"
                        |> Expect.equal True
            , test "invalid" <|
                \_ ->
                    validatePassPhrase1 "aa bb cc dd aa"
                        |> Expect.equal False
            , test "subwords ok" <|
                \_ ->
                    validatePassPhrase1 "aa bb cc dd aaa"
                        |> Expect.equal True
            ]
        , describe "validatePassPhrase2"
            [ test "valid" <|
                \_ ->
                    validatePassPhrase2 "abcde fghij"
                        |> Expect.equal True
            , test "invalid" <|
                \_ ->
                    validatePassPhrase2 "abcde xyz ecdab"
                        |> Expect.equal False
            , test "subwords ok" <|
                \_ ->
                    validatePassPhrase2 "a ab abc abd abf abj"
                        |> Expect.equal True
            , test "valid 2" <|
                \_ ->
                    validatePassPhrase2 "iiii oiii ooii oooi oooo"
                        |> Expect.equal True
            , test "invalid 2" <|
                \_ ->
                    validatePassPhrase2 "oiii ioii iioi iiio"
                        |> Expect.equal False
            ]
        , describe "numValid"
            [ test "validatePassPhrase1" <|
                \_ ->
                    numValid validatePassPhrase1 passPhrases1
                        |> Expect.equal 2
            , test "validatePassPhrase2" <|
                \_ ->
                    numValid validatePassPhrase2 passPhrases2
                        |> Expect.equal 3
            ]
        ]

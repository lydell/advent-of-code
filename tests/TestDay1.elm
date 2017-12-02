module TestDay1 exposing (..)

import Day1 exposing (inverseCaptcha1, inverseCaptcha2)
import Expect exposing (Expectation)
import Test exposing (..)


tests1 : List ( String, number )
tests1 =
    [ ( "1122", 3 )
    , ( "1111", 4 )
    , ( "1234", 0 )
    , ( "91212129", 9 )
    ]


tests2 : List ( String, number )
tests2 =
    [ ( "1212", 6 )
    , ( "1221", 0 )
    , ( "123425", 4 )
    , ( "123123", 12 )
    , ( "12131415", 4 )
    ]


suite : Test
suite =
    describe "Day1"
        [ describe "inverseCaptcha1" (runTests inverseCaptcha1 tests1)
        , describe "inverseCaptcha2" (runTests inverseCaptcha2 tests2)
        ]


runTests : (String -> Int) -> List ( String, Int ) -> List Test
runTests f tests =
    List.map (runTest f) tests


runTest : (String -> Int) -> ( String, Int ) -> Test
runTest f ( input, expected ) =
    test input <|
        \_ ->
            f input
                |> Expect.equal expected

module TestDay9 exposing (..)

import Day9 exposing (GarbageContent(Canceled, Chars), Stream(Garbage, Group), countGarbageChars, garbage, parse, score)
import Expect exposing (Expectation)
import Test exposing (..)


parseSuccess : List ( String, Stream, Int )
parseSuccess =
    [ ( "{<>}"
      , Group [ Garbage [] ]
      , 0
      )
    , ( "{<random characters>}"
      , Group [ Garbage [ Chars "random characters" ] ]
      , 17
      )
    , ( "{<<<<>}"
      , Group [ Garbage [ Chars "<<<" ] ]
      , 3
      )
    , ( "{<{!>}>}"
      , Group [ Garbage [ Chars "{", Canceled ">", Chars "}" ] ]
      , 2
      )
    , ( "{<!!>}"
      , Group [ Garbage [ Canceled "!" ] ]
      , 0
      )
    , ( "{<!!!>>}"
      , Group [ Garbage [ Canceled "!", Canceled ">" ] ]
      , 0
      )
    , ( "{<{o\"i!a,<{i<a>}"
      , Group [ Garbage [ Chars "{o\"i", Canceled "a", Chars ",<{i<a" ] ]
      , 10
      )
    , ( "{}"
      , Group []
      , 0
      )
    , ( "{{{}}}"
      , Group [ Group [ Group [] ] ]
      , 0
      )
    , ( "{{},{}}"
      , Group [ Group [], Group [] ]
      , 0
      )
    , ( "{{{},{},{{}}}}"
      , Group [ Group [ Group [], Group [], Group [ Group [] ] ] ]
      , 0
      )
    , ( "{<{},{},{{}}>}"
      , Group [ Garbage [ Chars "{},{},{{}}" ] ]
      , 10
      )
    , ( "{<a>,<a>,<a>,<a>}"
      , Group
            [ Garbage [ Chars "a" ]
            , Garbage [ Chars "a" ]
            , Garbage [ Chars "a" ]
            , Garbage [ Chars "a" ]
            ]
      , 4
      )
    , ( "{{<!>},{<!>},{<!>},{<a>}}"
      , Group
            [ Group
                [ Garbage
                    [ Canceled ">"
                    , Chars "},{<"
                    , Canceled ">"
                    , Chars "},{<"
                    , Canceled ">"
                    , Chars "},{<a"
                    ]
                ]
            ]
      , 13
      )
    ]


parseFail : List String
parseFail =
    [ "{<!>}"
    , "{<!!!>}"
    , "{"
    , "{<}>"
    , "{}trailing"
    , "{,}"
    ]


testParseSuccess : ( String, Stream, Int ) -> Test
testParseSuccess ( string, parsed, _ ) =
    test string <|
        \_ ->
            parse string
                |> Expect.equal (Ok parsed)


testParseFail : String -> Test
testParseFail string =
    test string <|
        \_ ->
            parse string
                |> Expect.err


scoreTests : List ( String, Int )
scoreTests =
    [ ( "{}", 1 )
    , ( "{{{}}}", 6 )
    , ( "{{},{}}", 5 )
    , ( "{{{},{},{{}}}}", 16 )
    , ( "{<a>,<a>,<a>,<a>}", 1 )
    , ( "{{<ab>},{<ab>},{<ab>},{<ab>}}", 9 )
    , ( "{{<!!>},{<!!>},{<!!>},{<!!>}}", 9 )
    , ( "{{<a!>},{<a!>},{<a!>},{<ab>}}", 3 )
    ]


testScore : ( String, Int ) -> Test
testScore ( string, expected ) =
    test string <|
        \_ ->
            let
                result =
                    string
                        |> parse
                        |> Result.map (score 0)
            in
            result
                |> Expect.equal (Ok expected)


testCountGarbageChars : ( String, Stream, Int ) -> Test
testCountGarbageChars ( string, _, expected ) =
    test string <|
        \_ ->
            let
                result =
                    string
                        |> parse
                        |> Result.map countGarbageChars
            in
            result
                |> Expect.equal (Ok expected)


suite : Test
suite =
    describe "Day9"
        [ describe "parse success" (List.map testParseSuccess parseSuccess)
        , describe "parse fail" (List.map testParseFail parseFail)
        , describe "score" (List.map testScore scoreTests)
        , describe "countGarbageChars" (List.map testCountGarbageChars parseSuccess)
        ]

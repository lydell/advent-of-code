module Day9 exposing (..)

import Day9Input exposing (input)
import Parser exposing ((|.), (|=), Count(Exactly), Error, Parser, end, ignore, keep, lazy, oneOf, oneOrMore, repeat, source, succeed, symbol, zeroOrMore)
import Parser.LanguageKit exposing (record)


output : () -> ( String, String )
output () =
    ( input |> parse |> Result.map (score 0) |> toString
    , input |> parse |> Result.map countGarbageChars |> toString
    )


type Stream
    = Group (List Stream)
    | Garbage (List GarbageContent)


type GarbageContent
    = Chars String
    | Canceled String


parse : String -> Result Error Stream
parse string =
    Parser.run (group |. end) string


group : Parser Stream
group =
    record (succeed ()) groupContent
        |> Parser.map Group


groupContent : Parser Stream
groupContent =
    oneOf
        [ lazy (\_ -> group)
        , garbage
        ]


garbage : Parser Stream
garbage =
    succeed Garbage
        |. symbol "<"
        |= repeat zeroOrMore garbageContent
        |. symbol ">"


garbageContent : Parser GarbageContent
garbageContent =
    oneOf
        [ garbageChars
        , canceledChar
        ]


garbageChars : Parser GarbageContent
garbageChars =
    ignore oneOrMore (\char -> char /= '!' && char /= '>')
        |> source
        |> Parser.map Chars


canceledChar : Parser GarbageContent
canceledChar =
    succeed Canceled
        |. symbol "!"
        |= keep (Exactly 1) (always True)


score : Int -> Stream -> Int
score nestingLevel stream =
    case stream of
        Group children ->
            nestingLevel
                + 1
                + List.sum (List.map (score (nestingLevel + 1)) children)

        Garbage _ ->
            0


countGarbageChars : Stream -> Int
countGarbageChars stream =
    case stream of
        Group children ->
            children
                |> List.map countGarbageChars
                |> List.sum

        Garbage garbageList ->
            garbageList
                |> List.map
                    (\item ->
                        case item of
                            Chars chars ->
                                String.length chars

                            Canceled _ ->
                                0
                    )
                |> List.sum

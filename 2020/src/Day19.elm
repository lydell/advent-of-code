module Day19 exposing (..)

import Day19Input exposing (puzzleInput)
import Dict exposing (Dict)
import Html exposing (Html)
import List.NonEmpty exposing (NonEmpty)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra


type Rule
    = MatchChar Char
    | Sequence (NonEmpty Int)


parse : String -> Result String ( Dict Int (NonEmpty Rule), List (List Char) )
parse input =
    case input |> String.trim |> String.split "\n\n" of
        [ top, bottom ] ->
            Result.map2 Tuple.pair
                (parseRules top)
                (parseLines bottom |> Ok)

        parts ->
            Err ("Expected 2 parts but got " ++ String.fromInt (List.length parts))


parseRules : String -> Result String (Dict Int (NonEmpty Rule))
parseRules =
    Parser.run (Parser.Extra.loopLineWise idWithRulesParser)
        >> Result.mapError Parser.Extra.deadEndsToString
        >> Result.map Dict.fromList


idWithRulesParser : Parser ( Int, NonEmpty Rule )
idWithRulesParser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.Extra.spaces
        |. Parser.symbol ":"
        |. Parser.Extra.spaces
        |= rulesParser
        |. Parser.Extra.spaces


rulesParser : Parser (NonEmpty Rule)
rulesParser =
    Parser.sequence
        { start = ""
        , separator = "|"
        , end = ""
        , spaces = Parser.Extra.spaces
        , item =
            Parser.oneOf
                [ Parser.map MatchChar charParser
                , Parser.map Sequence sequenceParser
                ]
        , trailing = Parser.Optional
        }
        |> Parser.andThen
            (\list ->
                case List.NonEmpty.fromList list of
                    Just nonEmpty ->
                        Parser.succeed nonEmpty

                    Nothing ->
                        Parser.problem "Unexpected empty list of rules"
            )


charParser : Parser Char
charParser =
    Parser.succeed identity
        |. Parser.symbol "\""
        |= Parser.getChompedString (Parser.chompWhile ((/=) '"'))
        |. Parser.symbol "\""
        |> Parser.andThen
            (\string ->
                case String.uncons string of
                    Just ( char, "" ) ->
                        Parser.succeed char

                    Just ( _, rest ) ->
                        Parser.problem
                            ("Expected a single char inside the string, but got "
                                ++ String.fromInt (String.length rest + 1)
                            )

                    Nothing ->
                        Parser.problem "Unexpected empty string."
            )


sequenceParser : Parser (NonEmpty Int)
sequenceParser =
    Parser.sequence
        { start = ""
        , separator = " "
        , end = ""
        , spaces = Parser.succeed ()
        , item = Parser.int
        , trailing = Parser.Optional
        }
        |> Parser.andThen
            (\list ->
                case List.NonEmpty.fromList list of
                    Just nonEmpty ->
                        Parser.succeed nonEmpty

                    Nothing ->
                        Parser.problem "Unexpected empty sequence"
            )


parseLines : String -> List (List Char)
parseLines =
    String.trim
        >> String.lines
        >> List.map String.toList


solution1 : String -> Result String Int
solution1 =
    parse >> Result.map solve


solution2 : String -> Result String Int
solution2 =
    parse >> Result.map (Tuple.mapFirst patchRulesDict >> solve)


patchRulesDict : Dict Int (NonEmpty Rule) -> Dict Int (NonEmpty Rule)
patchRulesDict =
    Dict.insert 8 ( Sequence ( 42, [] ), [ Sequence ( 42, [ 8 ] ) ] )
        >> Dict.insert 11 ( Sequence ( 42, [ 31 ] ), [ Sequence ( 42, [ 11, 31 ] ) ] )


solve : ( Dict Int (NonEmpty Rule), List (List Char) ) -> Int
solve ( rulesDict, lines ) =
    lines
        |> List.map (match 0 rulesDict >> evaluateNextResult)
        |> List.filter
            (\result ->
                case result of
                    Just [] ->
                        True

                    Just _ ->
                        False

                    Nothing ->
                        False
            )
        |> List.length


type Next
    = NoNext (List Char)
    | Next (List Char) (() -> Maybe Next)


evaluateNextResult : Maybe Next -> Maybe (List Char)
evaluateNextResult =
    Maybe.andThen
        (\next ->
            case next of
                NoNext remainingChars ->
                    Just remainingChars

                Next [] _ ->
                    Just []

                Next _ f ->
                    f () |> evaluateNextResult
        )


match : Int -> Dict Int (NonEmpty Rule) -> List Char -> Maybe Next
match ruleId rulesDict chars =
    Dict.get ruleId rulesDict
        |> Maybe.andThen (\rules -> matchRules rules rulesDict chars)


matchRules : NonEmpty Rule -> Dict Int (NonEmpty Rule) -> List Char -> Maybe Next
matchRules ( rule, restRules ) ruleDict chars =
    case matchRule rule ruleDict chars of
        Just (NoNext []) ->
            Just (NoNext [])

        Just (NoNext remainingChars) ->
            case restRules of
                [] ->
                    Just (NoNext remainingChars)

                first :: rest ->
                    Just
                        (Next remainingChars
                            (\() -> matchRules ( first, rest ) ruleDict chars)
                        )

        Just (Next [] _) ->
            Just (NoNext [])

        Just (Next remainingChars f) ->
            case restRules of
                [] ->
                    Just (Next remainingChars f)

                first :: rest ->
                    Just
                        (Next remainingChars
                            (\() -> matchRules ( first, rest ) ruleDict chars)
                            |> chainNext f
                        )

        Nothing ->
            case restRules of
                [] ->
                    Nothing

                first :: rest ->
                    matchRules ( first, rest ) ruleDict chars


matchRule : Rule -> Dict Int (NonEmpty Rule) -> List Char -> Maybe Next
matchRule rule rulesDict chars =
    case rule of
        MatchChar wantedChar ->
            case chars of
                [] ->
                    Nothing

                char :: rest ->
                    if char == wantedChar then
                        Just (NoNext rest)

                    else
                        Nothing

        Sequence ruleIds ->
            matchSequence ruleIds rulesDict chars


matchSequence : NonEmpty Int -> Dict Int (NonEmpty Rule) -> List Char -> Maybe Next
matchSequence ( ruleId, restRuleIds ) rulesDict chars =
    match ruleId rulesDict chars
        |> matchSequenceHelper restRuleIds rulesDict


matchSequenceHelper : List Int -> Dict Int (NonEmpty Rule) -> Maybe Next -> Maybe Next
matchSequenceHelper restRuleIds rulesDict =
    Maybe.andThen
        (\next ->
            case next of
                NoNext remainingChars ->
                    case restRuleIds of
                        [] ->
                            Just (NoNext remainingChars)

                        first :: rest ->
                            matchSequence ( first, rest ) rulesDict remainingChars

                Next remainingChars f ->
                    case restRuleIds of
                        [] ->
                            Just (Next remainingChars f)

                        first :: rest ->
                            case matchSequence ( first, rest ) rulesDict remainingChars of
                                Just next2 ->
                                    next2 |> chainNext f |> Just

                                Nothing ->
                                    f () |> matchSequenceHelper restRuleIds rulesDict
        )


chainNext : (() -> Maybe Next) -> Next -> Next
chainNext f next =
    case next of
        NoNext remainingChars ->
            Next remainingChars f

        Next remainingChars f2 ->
            Next remainingChars
                (\() ->
                    case f2 () of
                        Just next2 ->
                            next2 |> chainNext f |> Just

                        Nothing ->
                            f ()
                )


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
        , showResult (solution2 puzzleInput)
        ]


showResult : Result String Int -> Html msg
showResult result =
    Html.output []
        [ Html.text
            (case result of
                Ok int ->
                    String.fromInt int

                Err error ->
                    error
            )
        ]

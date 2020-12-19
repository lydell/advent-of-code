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
    parse
        >> Debug.log "parsed"
        >> Result.map
            (\( rulesDict, lines ) ->
                lines
                    |> List.map (match 0 rulesDict)
                    |> Debug.log "matches"
                    |> List.filter
                        (\result ->
                            case result of
                                Ok [] ->
                                    True

                                Ok _ ->
                                    False

                                Err _ ->
                                    False
                        )
                    |> List.length
            )


match : Int -> Dict Int (NonEmpty Rule) -> List Char -> Result String (List Char)
match ruleId rulesDict chars =
    case Dict.get ruleId rulesDict of
        Just rules ->
            matchHelper rules rulesDict chars

        Nothing ->
            Err ("Rule not found: " ++ String.fromInt ruleId)


matchHelper : NonEmpty Rule -> Dict Int (NonEmpty Rule) -> List Char -> Result String (List Char)
matchHelper ( rule, restRules ) ruleDict chars =
    case matchHelperHelper rule ruleDict chars of
        Ok remainingChars ->
            Ok remainingChars

        Err message ->
            case restRules of
                [] ->
                    Err ("No rule matched. Most recent error: " ++ message)

                first :: rest ->
                    matchHelper ( first, rest ) ruleDict chars


matchHelperHelper : Rule -> Dict Int (NonEmpty Rule) -> List Char -> Result String (List Char)
matchHelperHelper rule rulesDict chars =
    case rule of
        MatchChar wantedChar ->
            case chars of
                [] ->
                    Err "No more chars to consume for the current rule."

                char :: rest ->
                    if char == wantedChar then
                        Ok rest

                    else
                        Err ("Expected " ++ String.fromChar wantedChar ++ " but got " ++ String.fromChar char)

        Sequence ruleIds ->
            sequenceHelper ruleIds rulesDict chars


sequenceHelper : NonEmpty Int -> Dict Int (NonEmpty Rule) -> List Char -> Result String (List Char)
sequenceHelper ( ruleId, restRuleIds ) rulesDict chars =
    case match ruleId rulesDict chars of
        Ok remainingString ->
            case restRuleIds of
                [] ->
                    Ok remainingString

                first :: rest ->
                    sequenceHelper ( first, rest ) rulesDict remainingString

        Err message ->
            Err ("Sequence did not match: " ++ message)


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
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

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
        >> Result.map solve


solution2 : String -> Result String Int
solution2 =
    parse
        >> Result.map (Tuple.mapFirst patchRulesDict >> solve)


patchRulesDict : Dict Int (NonEmpty Rule) -> Dict Int (NonEmpty Rule)
patchRulesDict =
    Dict.insert 8 ( Sequence ( 42, [] ), [ Sequence ( 42, [ 8 ] ) ] )
        >> Dict.insert 11 ( Sequence ( 42, [ 31 ] ), [ Sequence ( 42, [ 11, 31 ] ) ] )


solve : ( Dict Int (NonEmpty Rule), List (List Char) ) -> Int
solve ( rulesDict, lines ) =
    lines
        |> List.map (match 0 rulesDict >> evaluateNextResult)
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


type Next
    = Next (List Char) (() -> Result String Next)
    | NoNext (List Char)


evaluateNextResult : Result String Next -> Result String (List Char)
evaluateNextResult next =
    case next of
        Ok (NoNext remainingChars) ->
            Ok remainingChars

        Ok (Next [] _) ->
            Ok []

        Ok (Next _ f) ->
            f () |> evaluateNextResult

        Err message ->
            Err message


match : Int -> Dict Int (NonEmpty Rule) -> List Char -> Result String Next
match ruleId rulesDict chars =
    case Dict.get ruleId rulesDict of
        Just rules ->
            matchHelper rules rulesDict chars

        Nothing ->
            Err ("Rule not found: " ++ String.fromInt ruleId)


matchHelper : NonEmpty Rule -> Dict Int (NonEmpty Rule) -> List Char -> Result String Next
matchHelper ( rule, restRules ) ruleDict chars =
    case matchHelperHelper rule ruleDict chars of
        Ok (NoNext []) ->
            Ok (NoNext [])

        Ok (NoNext remainingChars) ->
            case restRules of
                [] ->
                    Ok (NoNext remainingChars)

                first :: rest ->
                    Ok
                        (Next remainingChars
                            (\() -> matchHelper ( first, rest ) ruleDict chars)
                        )

        Ok (Next [] _) ->
            Ok (NoNext [])

        Ok (Next remainingChars f) ->
            case restRules of
                [] ->
                    Ok (Next remainingChars f)

                first :: rest ->
                    Ok
                        (Next remainingChars
                            (\() -> matchHelper ( first, rest ) ruleDict chars)
                            |> chainNext f
                        )

        Err message ->
            case restRules of
                [] ->
                    Err ("No rule matched. Most recent error: " ++ message)

                first :: rest ->
                    matchHelper ( first, rest ) ruleDict chars


matchHelperHelper : Rule -> Dict Int (NonEmpty Rule) -> List Char -> Result String Next
matchHelperHelper rule rulesDict chars =
    case rule of
        MatchChar wantedChar ->
            case chars of
                [] ->
                    Err "No more chars to consume for the current rule."

                char :: rest ->
                    if char == wantedChar then
                        Ok (NoNext rest)

                    else
                        Err ("Expected " ++ String.fromChar wantedChar ++ " but got " ++ String.fromChar char)

        Sequence ruleIds ->
            sequenceHelper ruleIds rulesDict chars


sequenceHelper : NonEmpty Int -> Dict Int (NonEmpty Rule) -> List Char -> Result String Next
sequenceHelper ( ruleId, restRuleIds ) rulesDict chars =
    match ruleId rulesDict chars
        |> sequenceHelperHelper restRuleIds rulesDict


sequenceHelperHelper : List Int -> Dict Int (NonEmpty Rule) -> Result String Next -> Result String Next
sequenceHelperHelper restRuleIds rulesDict next =
    case next of
        Ok (NoNext remainingChars) ->
            case restRuleIds of
                [] ->
                    Ok (NoNext remainingChars)

                first :: rest ->
                    sequenceHelper ( first, rest ) rulesDict remainingChars

        Ok (Next remainingChars f) ->
            case restRuleIds of
                [] ->
                    Ok (Next remainingChars f)

                first :: rest ->
                    case sequenceHelper ( first, rest ) rulesDict remainingChars of
                        Ok next2 ->
                            next2 |> chainNext f |> Ok

                        Err _ ->
                            f () |> sequenceHelperHelper restRuleIds rulesDict

        Err message ->
            Err ("Sequence did not match: " ++ message)


chainNext : (() -> Result String Next) -> Next -> Next
chainNext f next =
    case next of
        Next remainingChars f2 ->
            Next remainingChars
                (\() ->
                    case f2 () of
                        Ok next2 ->
                            next2 |> chainNext f |> Ok

                        Err _ ->
                            f ()
                )

        NoNext remainingChars ->
            Next remainingChars f


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

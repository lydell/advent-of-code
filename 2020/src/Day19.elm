module Day19 exposing (..)

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


puzzleInput : String
puzzleInput =
    """
108: 117 50 | 63 64
124: 50 64 | 64 64
123: 64 119 | 50 85
45: 60 64
32: 20 50 | 79 64
36: 97 64 | 70 50
97: 50 64 | 64 50
21: 50 104 | 64 83
59: 50 124 | 64 83
10: 115 64 | 34 50
118: 50 124 | 64 4
60: 12 50 | 4 64
84: 29 64 | 99 50
111: 64 27 | 50 14
18: 64 101 | 50 29
13: 64 45 | 50 1
62: 64 97 | 50 83
117: 124 64 | 70 50
98: 50 4 | 64 124
15: 64 114 | 50 12
7: 100 64 | 56 50
78: 99 50 | 52 64
82: 114 64 | 97 50
22: 6 64 | 4 50
63: 12 50 | 114 64
16: 64 52 | 50 70
76: 50 46 | 64 13
31: 64 69 | 50 67
37: 21 50 | 128 64
95: 68 64 | 16 50
12: 64 50 | 50 30
26: 50 12 | 64 83
112: 113 50 | 72 64
33: 104 64 | 52 50
25: 70 64 | 114 50
96: 51 64 | 105 50
51: 50 47 | 64 26
91: 103 50 | 9 64
65: 66 64 | 23 50
116: 64 114 | 50 97
42: 64 54 | 50 40
4: 50 64
79: 50 97 | 64 29
110: 64 116 | 50 118
0: 8 11
73: 50 12 | 64 101
70: 50 50 | 64 50
74: 61 50 | 65 64
27: 50 107 | 64 90
101: 30 30
105: 17 50 | 24 64
61: 58 64 | 102 50
17: 50 83 | 64 29
72: 30 97
88: 64 38 | 50 2
14: 50 36 | 64 2
125: 64 50 | 64 64
127: 95 64 | 28 50
83: 64 64
80: 64 107 | 50 81
106: 50 101 | 64 29
56: 88 64 | 49 50
52: 50 50 | 64 64
46: 80 50 | 120 64
94: 68 64 | 33 50
47: 50 114 | 64 6
69: 44 64 | 57 50
54: 74 64 | 3 50
89: 50 83
48: 106 64 | 59 50
87: 86 50 | 37 64
35: 64 73 | 50 55
115: 50 99 | 64 70
44: 111 50 | 109 64
75: 78 50 | 22 64
20: 64 101 | 50 6
41: 50 4 | 64 101
6: 64 50
66: 50 93 | 64 115
90: 64 101 | 50 124
23: 50 82 | 64 77
81: 50 114 | 64 101
30: 50 | 64
100: 122 50 | 75 64
86: 50 73 | 64 18
92: 64 48 | 50 71
68: 6 50 | 114 64
121: 50 70
8: 42
85: 64 112 | 50 110
38: 64 104 | 50 4
120: 84 50 | 55 64
67: 64 7 | 50 53
64: "a"
55: 50 52 | 64 125
28: 89 50 | 84 64
71: 50 39 | 64 16
109: 43 64 | 94 50
99: 50 50
43: 41 64 | 77 50
119: 64 108 | 50 35
57: 87 50 | 96 64
39: 64 97 | 50 6
104: 50 64 | 64 30
29: 50 64 | 50 50
34: 83 50 | 125 64
2: 99 64 | 124 50
11: 42 31
114: 64 64 | 30 50
93: 50 97 | 64 101
58: 50 33 | 64 121
113: 64 99 | 50 70
128: 64 4 | 50 125
40: 123 64 | 76 50
107: 50 52 | 64 83
102: 126 50 | 25 64
126: 50 125 | 64 97
19: 50 124 | 64 114
24: 114 64 | 29 50
49: 20 30
1: 64 19 | 50 15
103: 26 50 | 18 64
9: 64 117 | 50 62
50: "b"
53: 91 64 | 5 50
77: 6 64 | 99 50
5: 32 64 | 10 50
3: 64 92 | 50 127
122: 115 64 | 98 50

bbabbaaabbabbaaaabbbbbbb
bbaabaaaaabbbaaaabaaabba
baabababbbbaaaaabbabaaba
baaabbbabaabbaaababaabbaaaaabbbb
bbbbaabbbabaabbabaabaaaa
aabaaaabaababbbbababaabb
bbabbbaaabbbbabbbbabbabbbbbbbbabaabaaaabaabaabbabbabbbbbaabaaaba
baaaaaaabaabbbaaaaaababb
aaabbabaaabbabbbabaaabba
abbbbbabbababaabbaaabbbaababbbbaababbabb
bababbaabbbbbbaababaaabaaabababbabababba
babbaabbbabaabbbbbaaabba
abbbbbabbbbaababbababaaa
aaabaaababbaaabbbbbabbaaaabbbbba
bbaabaaaabbbaaaabbabbbab
baabaabababbaaababbababa
abbbbbabababaaabbbbbbbba
aababbbababbababaaababababbbbbbabaaabaaa
bbbbabaabbabaabbbbabbbaa
aabaabbbbbbababbbabbbaba
abbababaabbbaabbbabaaaababbbaabababbaaaaababbaabbbaabbabbbabaaabbaabaabbbbaaabaa
baababaaabababaabaaabbabbbaaaabbabbabaab
bbbaaaaabaababaaaaabbaaabbbababbaaabaaba
bbababbabbbbbbabbbbabbbbbaabbbabaabbbbababbabaab
abbbbbabbbabaabbbabbaabaaababaab
bbbbabbbbaababaaaaabbbbabbabbbbbbabbbbaaabaabbabaabbabbb
abbaabbabaaabaaaaabbbbaaaaabbbbb
aabaababababbbabbbabbbaabaaaabaaaaabbbbbaabbbabbaaabbbab
abaabaaabababbaababbabab
aabaaaabbbaababbbbabaaab
bbbababbabbbbabaaaabababbbabbbabbbbaaaabaaabbbbbaabababbbbababbbbbabaabb
aabbaababaabaaaaabbbbbabbbaaabaaaabaabab
baabbaaaaaaabbabbbababab
abbbabaabaabbbabbabaaaabaabaaaababaabbbb
aabbbabaaabababaabbababb
abababbbabaaabbaaaaaaababbabaabbbabbaaaabbbbbaab
abbabbaabbbaabbbabbaabaa
baabababbaabbaaabaaababb
aababaabbbabbababbaabababaaaabbabaabbaab
bbaaaabbbbbbababaabbbaaaabaabaaabbbbbbba
abbaabbbbaababaaaabaabaa
aaaaaabbabbaabbbbaabbabb
bababababababaabaaabbaabbababaababaabaabababbaba
ababbbbabbbbabaaabbbbbba
babaaabaaabbababbabbbbaa
bbabbbbabbbbbbabaaababbbaaabbabbbabbabbababbabbbbaabbbaabbababaa
bbaabaaaaabbbabaabbabbba
babababababbaabaaabbbbbb
baabbbabaabaabbabaabbbbb
babbbbababbaaabbbaaabbaabababbaabababaabbbbbaaabaaaaaaaa
aabbbabbababbbbaaaaaabaa
bbbababbababbbbabbaaabba
aabbabbaababaabbbbaababa
abbabaaabbbaaaaabbbababaaabbababbabbbaab
babaaaaaaababababbbabbbbbbbbbbaa
abaaababbabbbbbabaabaabb
babbbabbabbbbbabababbbbaaababbab
aaabbbbabababaabababbbbaabababab
baaabaabbabaaaabbababbaabbbabbbaabbbabbb
babaaabbabbabbabbabbbaba
bbaabbbababbbbabaaaabaaaaabbabababaabaab
abaaabaaabaabababbaaaabbbabbabab
bbbaaaababbabbabaaaaabab
bbbbababbaaabbaaabaabbbb
abbbbabbbaaaaababbbbbbba
bababbabaababbbbbababaaa
bbababbabbbbabbbbbababaabbbbbbbbabbbbabbbbaababa
baaaaababaabababaabaaabb
baabaababaaabbaaabababab
baaaaaaabbbababbbbbaabbbaababaaa
ababaaabbabaabbbabababba
baaabbbaabbabaaabbaaaaabaaaabbabbabbbbbbbbbaabaa
bbbababbabbbaaabaababaab
aaaaaabbaabbbaaabaaaaabb
babaaabbbbabbaaabbabbaab
aaabbaabbbababaaabaabaababababab
aaabaabbbabbaaaaababaaabababaabb
bbaaaabbbaaabbbaaababaaa
bbabbbbabbbaababbabbabba
baabababaaababaababaaaaaaababbab
baabbaaaaaabaaabbbbbaaaa
babbaaababaabaaaaaaabbbb
babaaaaaaaaabbabaabbababaabbabbaababaaabbbaabbaaaaabbbbb
abababaabbabaabbbaabbaaabbaaaaaa
bbbbbbaaaaabbabbbabbaaaabaabbbaababbbbbbbaabbbbaabbbababaabbaaababaababbabbbbaaaaabaaaaaaabaabab
bbababaabbbbaabbbbaabbbbbbbbbbbabbbaabbabbbbbabbabaaabaaaaabaaaa
abbbaabbbbbaabbbbabaabaabababbbbabaabbab
babaababaaababaabbaabbbbaaabaaabaaababbbabbbaaaabbabbbbbbbaababaabaabbbbbbbbbaaaaaababaa
babaabaababaabaabbaabbbb
bbbbbaaabbababaaabaabbaaaaaaabbbabbabaabbaaaabba
babbbbbbaaabbbabbbbabaaa
abaabaaababbbbabaababbab
bbaaabaabaaaaaaaaaabbbabaabbbbbb
aaabbbabaaaaaabbaabbaabbbaabaabaaabbababaaabaaababbbabab
aaabbaabaabbabaabbbbabbabaaabaabbaabbabb
babababaabaabaaababbaaba
babbbbbbabababaabaabbaba
baabbbabbbaaaaabababbaabaabbbbaaababaabaabbaabaaaaaabbaabbaaabba
abbbabaaabbbaabbbbaaabba
baabbbabaabbbbaaaabbbbbb
bababbabaaaabaaaaabbbbaaababaaaaaaababbaaaabbbbbabaabbab
abbaabbbbbabbbbbaabbbaab
ababbbabababbbaaaabbbabbbbaabaabbbbaaabbbaababbaaaaabbbbbaabbabb
aaababaaabbaabbbabbbbaaa
bbbababbbabbbaaaaabbbbaabaaabbaabbabababaaaaabaa
babaabaaabbaabbbbbbbabaaabbaaabbbaababbaabbbbbbbbaabaaaa
bababaabaabaaaabbaaaabba
baabbbbabaabababbaaabbabbbaabbbaabbbbabaababaaaaababbabaaabaabaaaaaababb
aabbbabbbbabbaaaaaaaabba
bbbaaabbaabbababbaabaaaa
abbbbbababbaabbbbaaabaaabbbbbabbbaaababaaabbaaababaabaaabbbabbaabbbbbbababaababb
babbbbbbababbbaabbabbbab
abbaabbaaababbbabbaababa
aaabbbabbabaabaaabbaabbbabbaabbbaaaaaaaa
abbbbabbbbbbabababbaabab
bbbbababaaabababbbbbbbba
bbbabaabababbbabbaaaabbbbababbabbabababa
aababababbbababbbbaabaaaababbababbbbaaaaabbbbabaabaababbabbbaaabbabbaaaabbbbababbabaabab
aabbaaabababbbaaabbaabaabababaaaaaaabababbabaaab
bbaaaaabbabbbbbabbabbbab
babbaababbbbabbbbbabaaab
aabababaaabbababbbbbaaba
babbaaaabbbaabbbbbbbabaaabbbbbba
bbaabaabbbabbbbaabaaababbaabababaabaaaaa
abaaababbababbaaabbbbbbb
bbaaaabbabbabaaaababbabb
aaabababaababababaaabbbb
ababbaaaabbbaabbbaabababbaaabbababbabbbabbaaabbb
aababbbaabababbabbabbbabbbaabababbbaababbabaabbabaababaa
baaabbbababbbabbbbaabaababbbbabbbabababbaabbbbba
aabababbaababbbbabbbaaaababbbbabbbbaaabbbabbbabaabaababb
baaabbabababbaaabaaaabab
aababbbaaabaabbaababbabb
aaabbaaaaababbbbbaabbbbbbaaaabaa
bbbbbaaababaabaaabbaabbbbbabbbabaaaaabab
aababbbbaabaabbabbabbaab
bbbaaaabaaaabaababbbbbbb
abbabaaaaaaabaaababaaabaaaababbbabaaabbb
abbbaaabbabababaabbbbbaa
baababaaaaababababababaabaaabbbaabaabaaa
bbbbbbbbababbbbabbabbaab
ababbaabaabaabbbabbbbbbb
babaaababaabbaaaabbabbaaababbababbabbbaaababbbabbbabbbabbabbbabb
babaabbabbbaaababbaaaaaa
bababbbaabababaabababaabaaaaaabbbbbababa
bbbbaabbababaababaaaaaab
ababbabbbabbbbaabaabbbbb
aabababbabababbaababaabbbbbaabaaabbbbbababbbbbabbbbbabbbbababbabbaababaa
ababbbababbabbaabaaaaabb
babaabbbbabbabbbaaabbaabbaabbbababbbabba
abbbaaabbaababaabbabaaba
bbbabaabbabaabbbaabaabab
ababbaaaaabaaaabbbaaabab
baababababbabbaabbababab
abbbbbabbbbbaabbaaaaaaab
baaaabbbaababaaaaabaaabbababbababaaaaabbbaaaabaa
bbbaaabbabbbaaaababaaaaa
bbbbaabbbbabbaaabbaaaaba
aaaabbabaaabbabbaaaabbba
abaabaaabbbaaaababbaaabaabbbaaabbbababaaaabbaabbabbabaababbabbbb
aaabaabbbbaabaababbbaaababbbaaba
baaabbbbbabbababbabaabab
babbbabbabbabaaaaaaaaabbbbaaaaaa
ababbaaaabaababaaaabbbbb
bbbaaabbbaabaababaaabbaabbbbbbaaabbbbbaa
ababaaaabbbbbbbbabaabbaa
bababaababbbabaaaabbabaa
babbabaababbaaabbabbbbaababaababbbbaaaaaaaabaabaaaaaababbbabbbbaaaabbbaa
baaabaaaabbbabbabaababbb
babaaaaabababababaabbaabbaabbbbabbabbabbbbaaaabababbabbbaabbbaaa
baabaababbaabbbaaababbab
aabaabababaabbbabbbabaaaabaaabaababbabbbaaabbbbbbaabbbaabaaabbbbaababbbbaabbaabb
aabbbabbbbaabaaabbbbbabb
aaabbaabaabbabbabaaaabba
baaabbbaaaabbaaababbabba
abaabbbababbbbababbaaaaa
bbaaabaaabbbbababbbaababbbbbbaaaabbaababaaabbbbb
ababaaaabbbbbaabbababaabbbabbaaabbbbaaba
aabbabaaaabbbabbbaabbbbaababaababbbabbbbabaaabaabbaaaabbaabaaaaa
bbbabbababbaaabbbaabbaaabaabbbaaaabbbbaabbababaaaabababb
babaaababbbbbaaaaabaaabb
abbbabbabbbbaabbbbabbbaa
bbbbabbbababbaabbbabaaaa
abbbaabbbbbbbaaaabbababa
bababaabbbbbbbbbaaaaabbb
babaaaababaabababbaabbaa
abbbaabbabaababaabaaabaaabbbabaabbaababa
bababbababbabbaabbaaaaaa
babbaaaababbaaabbbabaaaa
bbbbbaaaaabbababaaaaabaa
abaababaabaaabaaabaababb
baabbbbababbaabbbbbbbaba
bbabbbbbbbaaaaabbbaaaaabbaabbbaabbbbaaabaaaabbaa
aaabbbaaaaaaabbbaaabaabbaaaaaaaabbbaaabbbbbbbaba
aabbbababbababbaaaaaabbb
bbaabaaaabbaaabbabbabbababaaabbabbaaabba
babbaabbabbbbabbbabbbaaabbbaabbbbabbbaababaabbabbabbbbaa
bbbaabbbaababababaaaaaab
bbaaabaababaaabbaabbabbb
aaabaabbbbabbbbbababaabb
babaabbbbbbaababaabbbabbabbbabaabbabbaaababbabba
babbbbbabaaabbbaaaaaabab
aaabbbbabbababaaababbbbb
abbbaaaabaaaaababbabbaba
baabaaabbbbabbbbaabbbbaababaaabbabaaabba
ababaababaababaaaaabbbaa
baaabbabaaabbaabbbbbabaaaaaabbaaaaaabaaabaabaabaabaaabbaaabbababaaabbababbaaaaba
baabaabaaaaaaabbabbabaaabbabaabbbbabbbbabbabbaabbbabbbabbaabaabb
aabbbaaabbbabbbbbbbbbbba
aaabaabbaababbbabbabaaaa
aaabbabbabbbbbbaaabbbbbbabbbbaaa
aabbbbaaaabababaaababbbaaaabaaaa
bbaabaabbbbbbabababaababbbbbabaabbbbabaaabbabaab
aaabaabbaabbabbabaaaaaab
baaaaabaababbbabbbbaaaaa
ababbaabaaabaabbabaaaaba
baabaabaabababaababaaabbbbabbabb
baabbbabbbababbaaababaaa
aabaaaabaabaaaabbabababb
babbaababaabaaabaaaabbba
abbaabbbaabbbaaabaaababa
aabbbabbaababbbbbbaababbbbbbbbaaabaaaabb
babbabbaababababbaaaabbbbaaaaaabbaaabbbbababbbabaabababa
baabaaabbababbabbbaababa
babbbabbbbabbbbbabbababa
aabbababbbbbbbbbabbaabab
bbbbbaabaabbabbaabaaaabb
babbbbbbbabaaaabaabbbabbbabaabbbbbababaabbbbaabbabaaabbb
bbabbbbaaaababbabaaaaaab
abaaaabababaabbaababaabb
aaabbbabababbaaaaabaabbaababbbaabaaabbab
aaaabbabbababbbabaabbaab
bbaaaaabbbaabaababbabbba
baaabaaabaabbbbaaabaaabb
aababbbbbbbababbbaaabbbbbbaaabba
aaaabaaabbbbbbbaabaabbab
aabaabbabbbaabbbaabbabbababaabbababbaaabaabaabaaabaaabba
baababaabababababbbbbaba
bbaabaaabaaabbababbababa
abbbaaaaaaababaabaaabbaabaaabbaaaabbaaba
abbabbabbbbaaabaabaababb
babbabbbaabbbaaaaaaababb
bbbabababbaabaabaaabaaaa
bbbabaaabaababbabbaaabbaabaabbaa
baaaaababbbbbaabbbabbabb
baabababbaaabbbabababbaaababbabbabbbbbbb
abbbaabbabaaaaaaabaaaaaaaaabbbbababbaaaaababbababaabbbbbbabbbaababbbbaaa
aaabbabbbaaaaaaabababaababaaaaab
babaaaaaaabbbababbbbabbbabbaabbbaaabbaaaaaaaaaab
aabbbabbbababbaabbbababaabbaabbabaaaaabb
babbaaabbaabbbaabaabbabb
abbaabbaaabababbababbaba
abbbbbabbbbbbbbbbaabbabb
bbbaabbbbabaabaababaababbabbbabaaabaaabb
baabbbbabbbbbbbbbbbbaabbababbabb
aaabbbbaabbabbaaaaaababa
aabaabbabbbaaabababbbbbaaaaaaababaabbaba
abbabbabbaabbbabaaaabbaabaabbabbababbbbbbbaaaaaaaaaababbbbbaaabaaabaaaaabaaaabaa
aaababaaaabbababbaabbbaabaababaaaabaaabaaaaabbaa
baabaaabaaaaaaaaaabaaabbbbbbaaab
abaaabaaaaaaaabbbbaaaaba
babbaaabbbaaabaababbbaba
babbbbbabbaabaababbababbbabbabbabbbbbaba
aaaaaabbbaaaaaaaabaaaabaabbaabbaabbabbbbbbababbbaaaabbbb
bbbaababababbaabbbbabaaa
aaaaabbabbaaaaabaaabbaabbbabbabaababaabbababbaaababbabaabbbaaabbabbbbabb
babaabaaaabbbaaaaabbaaab
bbbbabbbbaaabbbabbbaababaaabbaabbabbbaba
aabaabaaabaaaabbabbabababaaabbabbbaaaabbbbbbaaaaababbaabaaabaabaabbababb
ababbaaaababbaabbaaaabbabababaababbaaabb
abababababbbabbbaabbbaababbabaab
babbbbabbbbababbbbbbbbabbbababbb
aababbbbabaaaabaaabaabab
bbbaaababababbababaababb
bbbabbbbbabbbabbaabaabab
baaababbaaabbbaaabbbabaaabaabaaaaaaabaaaabaababa
bbbbbbbbbbabbbbbbbbbbbabaaabbbaaabbbbaab
babbabbbbbbbabbbbbaaabba
babaaababbbbabaaaaabaaabaababababbabaaab
abaababaababbaaabbabbbbbabbabbab
bbbabbbbaabaaaabbbabaabbaaababbaaababbbbabaaaabbbbbabbbabaaaabab
babbbbbbaaabaabbbbbabbbbbabaabaaabaaabaabaaababa
abbabbababbbbababbabaaab
ababbaaaabaaabaabbaababb
aabaaaabababaaaabbbbaabbabbbaaaaabbaaaab
babaaaaaabbbaaaabababaababbbabbababaabaababbbbaaaabaabab
abaaaaaababbaabaabbaaaaababbaabbbabbbbbbbaaaabbbabaabbabbbabababaaababba
aabbbabababbaabbababbabb
aaabbaaaabaabbbaababaabb
bbaabbbabbbbababaaabbbabbbbbabbabaaaaaabaabbbbaaaaabbaabbaaaaaaa
babaaaabaaabbabbaaabbaba
aabbbbaabbbabbaabbbbbbba
bbaaaaaaabaabbaaaabaaaba
abbbaaabbabbbbbbbbbbabaababaaabbbbaabaaaabbbbbbaabaabbaaabbbababbbbabbba
abbaabbbaabbabababaaaabaabbababbabbbabbb
abbaaabbbbaabaaabbbbabbbbabbbbbbbaababbb
bbabbbbaabbbaaaabbbaababbbababbababbabba
abaaabaaabaabaaabbabbbaa
aaabbbabbbbbababaaabaaba
aabaabbabbbaaaabbabbabba
abbaaabbbbaabaabbababaaa
aabbabababbaaaaaaaaaabbbabababaabbbaaabbbbbaaaaa
aaaabbabbaababaabaaabaab
bababbaaaaabbaaaabbabbbb
abbaabbbabaaababbbbaababbabbaaaaabbbaaababbaaabaabbbbbba
aabbbabaaaabaaabbaaaabaa
bababbaabababbaabaabbaba
bbaabbbbbbbbaaaaabaabbbb
bbbbbbabbabbbbbaabbbbaaa
bababbbabaaabaaabbbabaab
baaaabaaaabbbbabbabbabbaaaaabbaa
bbbbbbabbaabbbabaabbbaab
baabaabaabbbaabbaababbab
aaabbaaabbbbababaabbababbabaabbaabaabbbabbbabaab
baaaaaababbbbbbabbabababbbaaabbbaabaaaababbbabbbaababaabbbaaaaabbaabbbbbabbababb
abbbabbabbaaabaabaaabaab
abaabbbabababbabababaabaabbabbabaabbaaaabbbbaaaa
bbabbabbbbbbaaabaabaababababaabbabababba
abaabbabbababaaabbabbababbabaaaababaaaaaaaaaabaabbbbaabbbbbbabaaaaaababbbbabaabaaababbba
bbaabbaabbbbbbabbabaaaabababaabbabbabbabbabaaabb
abbbaabbbbbaabbaabbbabab
bbbbbaabaaaabaabbaaababbabaaaabbabababbb
aabbbbbabaabaababaaabbaaaabaabbaabababbbaaabbbbb
aaaabaaabbbabababaaabbaaaabbabababbaababaababbaaabaaabba
bbbbabaaababbaaabbabaabbbabbaaaaaaaaaaab
babbaaaababbbbbabbbabababaabaabb
baabababbbbbabbbabbabbba
babaabaaaaabbaaaaaaaaaaa
bbbaaaabbabbaabbbabbbaba
abbaaabababbbaaabbaababb
aaababaabbabaabbbababaaa
baaabbaaababbabbbabaaaaaabaabbbbaaabaabaababbbbbbbaaaabbaabbbaabababbabbababbaaa
bbaabaabbbbababaaabbaabbaaabababbbbabaaaaaaaaaabbaaaaabbababbbbb
aaababbaabababaabbbaaabbbbabbaab
aabbbabaabbbabaabaabaaaa
ababbaaababbaababbabbaba
babbabaabbaaabbbabbbaaaaaabaabababaabaaaabaaabbbabbaabbbbabbabbbabbbaaab
ababbaaaababbaaabbbbaaba
bababbaabaabaaababbaabab
babaabbaaabaabbaabbbbabbaababbbabaaaabaaaababaab
babbbaaaabbbbbabaabaaaabbbabaaaa
abaabaaabababbabbbabaaab
babaabbabbbbabaaabaaaabaabbabbabaaaabbab
aaabaabbaabbbababbabbbbbaaabaaaa
ababaababbbaabababbaaaaabababbbbaabbaaba
aabbbabbabbabaaaabaabbab
bbabbaaabababbaabbababbb
baabbababbababbaaaaabbbbaabbbabaabaabbbbbabaabaabbbbbaaa
aababbbabaaaaaabbaaabaab
bbbbabbbaaaabaaaababaaaabbaaaaaaababbabb
baababbbaabaababbbababbbbbbaababbbbbaaabaaabbababbabaaaaababaaabaaaaabbabbbababb
baababaabbbabbbbbaaaabab
abbaaababbbaaabbbabababb
bbaaaaabbabbaabbababbabb
aabaabbaababaabaaaabbaba
bbbaababaaabbbbaabbaabbbbaaabababbbbbaba
bbaabbbabbaabaabaaaaabaa
aabaaaabbaaabbababaabbbabbbaaabbaabaaabaaaaabbba
babbaaabbaaabbaabbabbabb
bbbaababaaabbbabbabaabab
aaababaabbaaabaaaabaabbaabaabbab
aaababbaabaabaabbaaaaabbbbabbaaaaaaabbababbbaaaabaabbaabaabbaabbbbbabbab
aaaaaaaaabbaabaababbbbbababaababbabbaabbababbbabaaaaaabaabbbabab
baaaaabbabaaaabbaaababbabaabbaababbbbbabaaababbbbbbbbaaaabaaabababbababbabbabaababababab
abaaaaabbaabababbabaaaababbbaaaabbaaaaaa
ababaaaabbbbbaaaababaababaabbabbbbaaabab
bbaaaabbaabbbabbabbaabab
baabaabaababaabaaaabaaba
baabbbabbbaabbaabbababaabbbaaabbbaaaabaabaabbbbbaabbabaaabbababbbbbbbaabbabbaaabababaabb
aaaabaaaabbaaababbabbaba
aabababbaaabbbababaababb
ababaaabababbbabbbabbbab
abbbabbaaaababaabaaabbaaababbbbaaabbabbaaaaaabaabaaabbbbbbaaabbaaaaaabba
baaaabbbbaabbababbabbbabbabaaaba
baabaabaaaaabbabaabaabbbbaaaabbaabaaabbb
abbaabbbababbaaababaaababbbaaababaaaaaabbbababab
aabbbbaaaaaabaababbaaaababaaabbbaabbbaaaabbbbbababbbaabaaaaabbabaaaaaaaa
baaaaaaabababaabababbaba
bababbbaaabbababbbaaaaba
aaababbaaabbabbaababbbbb
aabababbabbbbabaabbabaaaabaabbbabbababbabababbabaaaaabba
abbababbbaabaabbbaabbaaaabaaabbaababbbbbbbbbbabbaababbaabaabaababbbabbabaabbabbbbbaaaabbbbbababb
aabbabbaabaababaaaaaabba
aabbababaaababbaabaabbab
abbbbbaababbaababbababaabbaaaaaabaaabababaaaabbbbbaaabbb
aaaaabbabaababbaaabaabaa
babbbaaabaabbbbababaabbaabaaaaabbbabbbaaabbbabbbbabbbbaa
aaaabaabbabaaaababababba
abbbabbbaaaaabbbbbaabbaa
bbbaaaabbbaabbbababaaabbbbababaabbaaaaaa
aaaabaabbbbaaaabbbbabbab
aababbbbbaabababbbaaabba
baabbbbababbbabbaabbabbabbbababbaaaaabab
bbabbbbabaabbaaabaabbbbb
ababbaabbbababbabbababab
aaabababbbbababbabaaababbbabaaba
aaabababbaaabbbabbabbabb
bbbbbbabaabaaaabbbaaabab
abbabbaabbbabbaaabbaabbbaabaababaababbab
abbaabbababbaaaaabbaabbaaabaabbaaabbaaba
ababbbabbaababbbbabbbbaaaabbaaabaaaaaaab
baabbbbaaaababbbaabbbbbaaabaaaaa
bbbababababbaabaaaaaabaa
baabababbbbaabbababbbbbaaaababbaabaabbbaaabbabaabbaabbbb
aabbbaaabbbbbaabaaabbaba
babbbabbaabbaabbbaabbaab
bbbabaabbbbaabbaaaabbabbaaabababbbabbaaaabbbaabbbaaabaaababababb
bbbaaaabbbabbbbabbbabbba
aabbbabababaaaaabbbabbaaaabaaaba
bababbaababababaabaaaabb
bbbaaabbbbbaabbabbbbbbaabbaabbbb
abbbaabbbababaabbbaaaaba
abababaabaaabbbabaaabbabbabaaaabbbaaabab
bbbabbbbbbbaaaaabbbbababbbabaaabababbbbb
abaaaaaaababbaababbaaabaabbaaaab
bababaababbbabbabbabaaba
babbaababababaabbbbbbbbbabbababbbababbbb
bbbaaabbbbababaababbbaba
aaabbbbaabbbababbababaaaaaabbbbaababbbbbabaaababbbabbbaaaaaaaabaaaaaabaa
bbaabbbabbbbbbaaaaaaaaab
baaabbbabbbaaaaaaabbaaaa
aaaabaaaaabaabbaabbabbbb
abbbbabbbaababaaabababbbbaaabaabaaaaabaabbbbabbaabbabbbb
aaaabaabbbababbababbaabbbbabaabbbbbbaaab
bbbbbaaabbbaababbbbababbbbabaabbaabbbbba
aaabbbbababbbabbbbbaababbbbbbbbbbbabaabbaaaaabaa
aabbbabaababaaabbabbbaba
bbbaababbbbbbbbbabaabbaa
ababaaababbbbbabaaaaabab
bbbababbbababbbaabbababa
bababbbaabbbabbababbabaa
bbbababaabaaaabaaaaabbbb
ababaaabbbbaaaabbbabaaba
aaababaabbbaaabbbbbaaaababbabbbb
babbbbabbababbaaababaaaaabaababbabbbabbb
bababbabbabbabbbaabbababbaabbaaaaabbbaabaabbaabbbbaaabbaabbabbbabbbbbaab
babbaababaaabbaaababaaaabbbaabaa
bbabbbbabaabababbaaabaab
abbbaaabaaababbaabaabbab
bbbababbbbbabbbbbbabbabb
bbbbbaababaaaabaababaabb
abbabaaaabbbaaaababbbbab
ababbbabaaabbbabaabbbbaaaaabbbbbaabaaaba
bbaaabaaaaababaaabaabbbb
abaaabaababaaabbbaaabbaabbbbabaabbabbaab
bbbbbababbabaababaababbbaabbaabaaabaaaaaaabbbbba
aabbbabbabbaaababbababaabbbabababaaaaaaaaabaabaabaababba
abaaaababbbbbbababaabbbabbbaaaaaababbabb
babbbbbaaaaabbabbaaaaabb
aaabaababbaabbaaabbbbbabbbbababbaabbbbbababbbaab
abbabaaabbbabbaababbaabbaabbbbba
babaaabaabbbabbaaaaabbaa
baabbbbabaaaaababbbaabbbaaaabbbb
aabbaabbbbbaaaabbbbabbaaabbabbababbbbaababaababb
abaaaabbbbaaababbbbbbabb
abaaaabaababbbabaabaaabb
abaababbaabaababbaaabbbb
bbabbabbabababbaaaabbabaabababab
aaaaaabbbabaaaabbbbbbbaabaaaaabbaababbabbabbbababaaaabba
bbababbabaaababbababbbbbbabbabab
bbabbaaababbbbaababbaaab
baaaaaaaaabababbaabaabaa
abbaaabbabbbaaaababaaabaababaabaababaabb
abaaaaabaaababbaaaabaaab
abaaaaaabbaabaaaaaabbaabababbbaaabbbabbbbaabbaab
abbaaabbbbaabbaaaabbbaaabaabaaaa
aabaaaabbbbaaaabaaababbb
abbaabaabbbbababbabbaaababaaabbaaabaaaabbabababababababb
"""

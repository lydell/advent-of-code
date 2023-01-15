module Day22 exposing (..)

import Deque exposing (Deque)
import Html exposing (Html)
import LineParser
import Set exposing (Set)


parse : String -> Result String ( Deque Int, Deque Int )
parse input =
    case input |> String.trim |> String.split "\n\n" of
        [ first, second ] ->
            Result.map2 Tuple.pair
                (parseCards first |> Result.mapError ((++) "Player 1: "))
                (parseCards second |> Result.mapError ((++) "Player 2: "))

        parts ->
            Err ("Expected 2 parts but got " ++ String.fromInt (List.length parts))


parseCards : String -> Result String (Deque Int)
parseCards =
    String.lines
        >> List.drop 1
        >> String.join "\n"
        >> LineParser.parse (String.toInt >> Result.fromMaybe "Not a number.")
        >> Result.map Deque.fromList


solution1 : String -> Result String Int
solution1 =
    parse >> Result.map (playCombat >> score)


score : Deque Int -> Int
score =
    Deque.toList
        >> List.reverse
        >> List.indexedMap (\index number -> number * (index + 1))
        >> List.sum


playCombat : ( Deque Int, Deque Int ) -> Deque Int
playCombat decks =
    let
        ( next1, next2 ) =
            playOneRoundCombat decks
    in
    if Deque.isEmpty next1 then
        next2

    else if Deque.isEmpty next2 then
        next1

    else
        playCombat ( next1, next2 )


playOneRoundCombat : ( Deque Int, Deque Int ) -> ( Deque Int, Deque Int )
playOneRoundCombat ( player1, player2 ) =
    case ( Deque.popFront player1, Deque.popFront player2 ) of
        ( ( Just first1, rest1 ), ( Just first2, rest2 ) ) ->
            if first1 > first2 then
                ( rest1 |> Deque.pushBack first1 |> Deque.pushBack first2
                , rest2
                )

            else
                ( rest1
                , rest2 |> Deque.pushBack first2 |> Deque.pushBack first1
                )

        _ ->
            ( player1, player2 )


solution2 : String -> Result String Int
solution2 =
    parse >> Result.map (playRecursiveCombat >> Tuple.second >> score)


type alias History =
    ( Set String, Set String )


type RoundResult
    = Continue History ( Deque Int, Deque Int )
    | InfiniteRecursionPrevention (Deque Int)


type Player
    = Player1
    | Player2


playRecursiveCombat : ( Deque Int, Deque Int ) -> ( Player, Deque Int )
playRecursiveCombat =
    playRecursiveCombatHelper ( Set.empty, Set.empty ) >> Tuple.second


playRecursiveCombatHelper : History -> ( Deque Int, Deque Int ) -> ( History, ( Player, Deque Int ) )
playRecursiveCombatHelper history decks =
    case playOneRoundRecursiveCombat history decks of
        Continue nextHistory ( next1, next2 ) ->
            if Deque.isEmpty next1 then
                ( nextHistory, ( Player2, next2 ) )

            else if Deque.isEmpty next2 then
                ( nextHistory, ( Player1, next1 ) )

            else
                playRecursiveCombatHelper nextHistory ( next1, next2 )

        InfiniteRecursionPrevention next1 ->
            ( history, ( Player1, next1 ) )


playOneRoundRecursiveCombat : History -> ( Deque Int, Deque Int ) -> RoundResult
playOneRoundRecursiveCombat ( history1, history2 ) ( player1, player2 ) =
    let
        player1String =
            deckToString player1

        player2String =
            deckToString player2
    in
    if Set.member player1String history1 || Set.member player2String history2 then
        InfiniteRecursionPrevention player1

    else
        case ( Deque.popFront player1, Deque.popFront player2 ) of
            ( ( Just first1, rest1 ), ( Just first2, rest2 ) ) ->
                let
                    winner =
                        if Deque.length rest1 >= first1 && Deque.length rest2 >= first2 then
                            playRecursiveCombat
                                ( Deque.left first1 rest1
                                , Deque.left first2 rest2
                                )
                                |> Tuple.first

                        else if first1 > first2 then
                            Player1

                        else
                            Player2

                    nextHistory =
                        ( Set.insert player1String history1
                        , Set.insert player2String history2
                        )
                in
                case winner of
                    Player1 ->
                        Continue nextHistory
                            ( rest1 |> Deque.pushBack first1 |> Deque.pushBack first2
                            , rest2
                            )

                    Player2 ->
                        Continue nextHistory
                            ( rest1
                            , rest2 |> Deque.pushBack first2 |> Deque.pushBack first1
                            )

            _ ->
                Continue ( history1, history2 ) ( player1, player2 )


deckToString : Deque Int -> String
deckToString =
    Deque.toList
        >> List.map String.fromInt
        >> String.join ","


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
Player 1:
41
26
29
11
50
38
42
20
13
9
40
43
10
24
35
30
23
15
31
48
27
44
16
12
14

Player 2:
18
6
32
37
25
21
33
28
7
8
45
46
49
5
19
2
39
4
17
3
22
1
34
36
47
"""

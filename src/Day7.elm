module Day7 exposing (..)

import Day7Input exposing (puzzleInput)
import Dict exposing (Dict)
import Graph exposing (Graph)
import Graph.DOT
import Html exposing (Html)
import Html.Attributes
import IntDict
import LineParser
import List.Extra as List


type alias Bag =
    -- ( "shiny", "gold" ) for example
    ( String, String )


type alias BagCount =
    Int


parse : String -> Result String ( Dict Bag Graph.NodeId, Graph Bag BagCount )
parse =
    parseLines >> Result.map makeGraph


parseLines : String -> Result String (List ( Bag, List ( BagCount, Bag ) ))
parseLines =
    LineParser.parse
        (\line ->
            case String.split " contain " line of
                [ before, after ] ->
                    let
                        left =
                            parseBag (String.words before)

                        right =
                            if after == "no other bags." then
                                Ok []

                            else
                                after
                                    |> removeEnding "."
                                    |> String.split ","
                                    |> LineParser.parseGeneral "Segment"
                                        (\part ->
                                            case String.words part of
                                                [] ->
                                                    Err "Unexpected empty segment."

                                                first :: rest ->
                                                    String.toInt first
                                                        |> Result.fromMaybe "First word is not a number."
                                                        |> Result.andThen
                                                            (\bagCount ->
                                                                parseBag rest
                                                                    |> Result.map (Tuple.pair bagCount)
                                                            )
                                        )
                    in
                    Result.map2 Tuple.pair
                        left
                        right

                parts ->
                    Err
                        ("Expected 2 parts separated by 'contain' but got "
                            ++ String.fromInt (List.length parts)
                            ++ ": "
                            ++ String.join " /// " parts
                        )
        )


parseBag : List String -> Result String Bag
parseBag words =
    case words of
        [ modifier, color, "bag" ] ->
            Ok ( modifier, color )

        [ modifier, color, "bags" ] ->
            Ok ( modifier, color )

        _ ->
            Err ("Expected `modifier color bag(s)` but got: " ++ String.join " " words)


removeEnding : String -> String -> String
removeEnding end string =
    if String.endsWith end string then
        String.dropRight (String.length end) string

    else
        string


makeGraph : List ( Bag, List ( BagCount, Bag ) ) -> ( Dict Bag Graph.NodeId, Graph Bag BagCount )
makeGraph lines =
    let
        idDict : Dict Bag Graph.NodeId
        idDict =
            lines
                |> List.indexedMap (\index ( bag, _ ) -> ( bag, index ))
                |> Dict.fromList

        getId : Bag -> Graph.NodeId
        getId bag =
            Dict.get bag idDict |> Maybe.withDefault -1

        nodes : List (Graph.Node Bag)
        nodes =
            lines
                |> List.map (\( bag, _ ) -> Graph.Node (getId bag) bag)

        edges : List (Graph.Edge BagCount)
        edges =
            lines
                |> List.concatMap
                    (\( bag, bagsWithCounts ) ->
                        bagsWithCounts
                            |> List.map
                                (\( bagCount, subBag ) ->
                                    Graph.Edge (getId bag) (getId subBag) bagCount
                                )
                    )
    in
    ( idDict
    , Graph.fromNodesAndEdges nodes edges
    )


solution1 : String -> Result String Int
solution1 =
    parse
        >> Result.andThen
            (\( idDict, graph ) ->
                Dict.get ( "shiny", "gold" ) idDict
                    |> Result.fromMaybe "shiny gold not found"
                    |> Result.map
                        (\id ->
                            Graph.guidedDfs
                                Graph.alongIncomingEdges
                                (Graph.onFinish (::))
                                [ id ]
                                []
                                graph
                                |> Tuple.first
                                |> List.drop 1
                                |> List.length
                        )
            )


solution2 : String -> Result String Int
solution2 =
    parse
        >> Result.andThen
            (\( idDict, graph ) ->
                Dict.get ( "shiny", "gold" ) idDict
                    |> Result.fromMaybe "shiny gold not found"
                    |> Result.map (countBags graph)
            )


countBags : Graph Bag Int -> Graph.NodeId -> Int
countBags graph id =
    Graph.get id graph
        |> Maybe.map
            (.outgoing
                >> IntDict.toList
                >> List.map
                    (\( subId, count ) ->
                        count * (1 + countBags graph subId)
                    )
                >> List.sum
            )
        |> Maybe.withDefault 0


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

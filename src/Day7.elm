module Day7 exposing (..)

import Day7Input exposing (puzzleInput)
import Dict exposing (Dict)
import Graph exposing (Graph)
import Graph.DOT
import Html exposing (Html)
import Html.Attributes
import LineParser
import List.Extra as List


type alias Bag =
    ( String, String )


parse : List ( Bag, List ( Int, Bag ) ) -> ( Bag -> Int, Graph Bag Int )
parse lines =
    let
        idDict : Dict Bag Int
        idDict =
            lines
                |> List.indexedMap (\index ( bag, _ ) -> ( bag, index ))
                |> Dict.fromList

        getId : Bag -> Int
        getId bag =
            Dict.get bag idDict |> Maybe.withDefault -1

        nodes =
            lines
                |> List.map (\( bag, _ ) -> Graph.Node (getId bag) bag)

        edges =
            lines
                |> List.concatMap
                    (\( bag, bagsWithNumbers ) ->
                        let
                            fromId =
                                getId bag
                        in
                        bagsWithNumbers
                            |> List.map
                                (\( number, subBag ) ->
                                    let
                                        toId =
                                            getId subBag
                                    in
                                    Graph.Edge fromId toId number
                                )
                    )
    in
    ( getId
    , Graph.fromNodesAndEdges nodes edges
    )


parseLines : String -> Result String (List ( Bag, List ( Int, Bag ) ))
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
                                                            (\number ->
                                                                parseBag rest
                                                                    |> Result.map (Tuple.pair number)
                                                            )
                                        )
                    in
                    Result.map2 Tuple.pair
                        left
                        right

                parts ->
                    Err ("Expected 2 parts but got " ++ String.fromInt (List.length parts) ++ ": " ++ String.join " /// " parts)
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



-- solution1_old : String -> Result String Int
-- solution1_old =
--     parseLines
--         >> Result.map parse
--         >> Result.andThen
--             (Graph.checkAcyclic
--                 >> Result.mapError
--                     (\{ from, to, label } ->
--                         "Graph is cyclic: "
--                             ++ String.fromInt from
--                             ++ "->"
--                             ++ String.fromInt to
--                             ++ " ("
--                             ++ String.fromInt label
--                             ++ ")"
--                     )
--             )
--         >> Result.map
--             (Graph.heightLevels
--                 >> List.map (List.map (.node >> .label))
--                 >> List.takeWhile (not << List.member ( "shiny", "gold" ))
--                 >> List.concat
--                 >> List.length
--             )
-- solution1 : String -> Result String (List a)


solution1 =
    parseLines
        >> Result.map
            (\lines ->
                let
                    ( getId, graph ) =
                        parse lines

                    answer =
                        Graph.guidedDfs Graph.alongIncomingEdges (Graph.onFinish (::)) [ getId ( "shiny", "gold" ) ] [] graph
                            |> Tuple.first
                            |> List.drop 1
                            |> List.map (.node >> .label)
                            |> List.length
                in
                ( answer, graph )
            )


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
        ]


showResult : Result String ( a, Graph Bag Int ) -> Html msg
showResult result =
    Html.output []
        (case result of
            Ok ( x, graph ) ->
                [ Html.div [] [ Html.text (Debug.toString x) ]
                , Html.div [ Html.Attributes.style "margin-top" "50px" ]
                    [ Html.text
                        (Graph.DOT.output
                            (\( a, b ) -> a ++ " " ++ b |> Just)
                            (String.fromInt >> Just)
                            graph
                        )
                    ]
                ]

            Err error ->
                [ Html.text error ]
        )


shortInput : String
shortInput =
    """
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""

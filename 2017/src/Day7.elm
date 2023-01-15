module Day7 exposing (..)

import Day7Input exposing (input)
import List.Extra
import Regex exposing (HowMany(..), Match, Regex)
import Set


output : () -> ( String, String )
output () =
    ( input |> parse |> findBottomNodeNames |> toString
    , input |> parse |> createTrees |> findBalancingWeight |> toString
    )


type alias Node =
    { name : String
    , weight : Int
    , children : List String
    }


type Tree
    = TreeNode
        { name : String
        , weight : Int
        , children : List Tree
        }


lineRegex : Regex
lineRegex =
    Regex.regex "^([a-z]+) \\((\\d+)\\)(?: -> ([a-z]+(?:, [a-z]+)*))?$"


parse : String -> List Node
parse string =
    string
        |> String.lines
        |> List.filterMap parseLine


parseLine : String -> Maybe Node
parseLine string =
    string
        |> Regex.find (AtMost 1) lineRegex
        |> List.head
        |> Maybe.andThen parseMatch


parseMatch : Match -> Maybe Node
parseMatch { submatches } =
    case submatches of
        [ Just name, Just weightString, maybeChildren ] ->
            case String.toInt weightString of
                Ok weight ->
                    Just
                        { name = name
                        , weight = weight
                        , children =
                            maybeChildren
                                |> Maybe.map (String.split ", ")
                                |> Maybe.withDefault []
                        }

                Err _ ->
                    Nothing

        _ ->
            Nothing


findBottomNodes : List Node -> List Node
findBottomNodes nodeList =
    let
        nonBottomNames =
            nodeList
                |> List.concatMap .children
                |> Set.fromList

        isBottomNode node =
            (List.length node.children > 0)
                && not (Set.member node.name nonBottomNames)
    in
    List.filter isBottomNode nodeList


findBottomNodeNames : List Node -> List String
findBottomNodeNames =
    findBottomNodes >> List.map .name


createTrees : List Node -> List Tree
createTrees nodeList =
    nodeList
        |> findBottomNodes
        |> createTreesHelper nodeList


createTreesHelper : List Node -> List Node -> List Tree
createTreesHelper allNodes bottomNodes =
    bottomNodes
        |> List.map
            (\node ->
                TreeNode
                    { name = node.name
                    , weight = node.weight
                    , children =
                        node.children
                            |> List.filterMap (findNode allNodes)
                            |> createTreesHelper allNodes
                    }
            )


findNode : List Node -> String -> Maybe Node
findNode nodeList name =
    nodeList
        |> List.Extra.find (.name >> (==) name)


treeWeight : Tree -> Int
treeWeight (TreeNode { weight, children }) =
    let
        childrenWeight =
            children
                |> List.map treeWeight
                |> List.sum
    in
    weight + childrenWeight


findUnbalanced : Tree -> List ( String, Int )
findUnbalanced (TreeNode { name, weight, children }) =
    let
        unbalancedChildren =
            List.concatMap findUnbalanced children

        childrenWithTreeWeights =
            children
                |> List.map (\child -> ( child, treeWeight child ))

        childrenWeights =
            List.map Tuple.second childrenWithTreeWeights

        offendingChild =
            childrenWithTreeWeights
                |> List.Extra.minimumBy
                    (\( child, childWeight ) ->
                        childrenWeights
                            |> List.filter ((==) childWeight)
                            |> List.length
                    )

        maxChildWeight =
            childrenWeights |> List.maximum |> Maybe.withDefault 0

        minChildWeight =
            childrenWeights |> List.minimum |> Maybe.withDefault 0

        childWeightDiff =
            maxChildWeight - minChildWeight
    in
    if childWeightDiff == 0 then
        unbalancedChildren

    else
        case offendingChild of
            Just ( TreeNode child, _ ) ->
                ( child.name, child.weight - childWeightDiff ) :: unbalancedChildren

            Nothing ->
                unbalancedChildren


findBalancingWeight : List Tree -> Maybe Int
findBalancingWeight =
    List.concatMap findUnbalanced
        >> List.Extra.last
        >> Maybe.map Tuple.second

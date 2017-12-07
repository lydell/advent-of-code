module TestDay7 exposing (..)

import Day7 exposing (Node, createTrees, findBottomNodeNames, findUnbalanced, parse)
import Expect exposing (Expectation)
import Test exposing (..)


input : String
input =
    """pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"""


parsed : List Node
parsed =
    [ { name = "pbga", weight = 66, children = [] }
    , { name = "xhth", weight = 57, children = [] }
    , { name = "ebii", weight = 61, children = [] }
    , { name = "havc", weight = 66, children = [] }
    , { name = "ktlj", weight = 57, children = [] }
    , { name = "fwft", weight = 72, children = [ "ktlj", "cntj", "xhth" ] }
    , { name = "qoyq", weight = 66, children = [] }
    , { name = "padx", weight = 45, children = [ "pbga", "havc", "qoyq" ] }
    , { name = "tknk", weight = 41, children = [ "ugml", "padx", "fwft" ] }
    , { name = "jptl", weight = 61, children = [] }
    , { name = "ugml", weight = 68, children = [ "gyxo", "ebii", "jptl" ] }
    , { name = "gyxo", weight = 61, children = [] }
    , { name = "cntj", weight = 57, children = [] }
    ]


suite : Test
suite =
    describe "Day7"
        [ describe "parse"
            [ test "it works" <|
                \_ ->
                    parse input
                        |> Expect.equalLists parsed
            ]
        , describe "findBottomNodeNames"
            [ test "it works" <|
                \_ ->
                    findBottomNodeNames parsed
                        |> Expect.equalLists [ "tknk" ]
            ]
        , describe "findUnbalanced"
            [ test "it works" <|
                \_ ->
                    List.concatMap findUnbalanced (createTrees parsed)
                        |> Expect.equalLists [ ( "ugml", 60 ) ]
            ]
        ]

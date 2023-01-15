module TestDay12 exposing (..)

import Day12 exposing (findAllGroups, findGroup, parse)
import Expect exposing (Expectation)
import Set exposing (Set)
import Test exposing (..)


input : String
input =
    """0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"""


group : Set Int
group =
    Set.fromList [ 0, 2, 3, 4, 5, 6 ]


allGroups : List (Set Int)
allGroups =
    [ Set.fromList [ 1 ]
    , group
    ]


suite : Test
suite =
    describe "Day12"
        [ describe "findGroup"
            [ test "it works" <|
                \_ ->
                    findGroup 0 (parse input)
                        |> Expect.equalSets group
            ]
        , describe "findAllGroups"
            [ test "it works" <|
                \_ ->
                    findAllGroups (parse input)
                        |> Expect.equalLists allGroups
            ]
        ]

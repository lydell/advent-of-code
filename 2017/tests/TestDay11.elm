module TestDay11 exposing (..)

import Day11 exposing (Direction(N, NE, NW, S, SE, SW), distances)
import Expect exposing (Expectation)
import Test exposing (..)


distancesTests : List ( List Direction, Int, Int )
distancesTests =
    [ ( [ NE, NE, NE ], 3, 3 )
    , ( [ NE, NE, SW, SW ], 0, 2 )
    , ( [ NE, NE, S, S ], 2, 2 )
    , ( [ SE, SW, SE, SW, SW ], 3, 3 )
    ]


testDistances : ( List Direction, Int, Int ) -> Test
testDistances ( directions, fromStart, maxFromStart ) =
    test (toString directions) <|
        \_ ->
            distances directions
                |> Expect.equal
                    { fromStart = fromStart
                    , maxFromStart = maxFromStart
                    }


suite : Test
suite =
    describe "Day11"
        [ describe "distances" (List.map testDistances distancesTests)
        ]

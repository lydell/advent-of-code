module TestDay8 exposing (..)

import Day8 exposing (ChangeType(..), Instruction, Operator(..), Registry, evaluate, largestRegisterValue, parse)
import Dict
import Expect exposing (Expectation)
import Test exposing (..)


input : String
input =
    """b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""


parsed : List Instruction
parsed =
    [ { register = "b"
      , changeType = Inc
      , changeValue = 5
      , condition =
            { operator = O_GT
            , register = "a"
            , value = 1
            }
      }
    , { register = "a"
      , changeType = Inc
      , changeValue = 1
      , condition =
            { operator = O_LT
            , register = "b"
            , value = 5
            }
      }
    , { register = "c"
      , changeType = Dec
      , changeValue = -10
      , condition =
            { operator = O_GTE
            , register = "a"
            , value = 1
            }
      }
    , { register = "c"
      , changeType = Inc
      , changeValue = -20
      , condition =
            { operator = O_EQ
            , register = "c"
            , value = 10
            }
      }
    ]


evaluated : Registry
evaluated =
    { registers =
        Dict.fromList
            [ ( "a", 1 )
            , ( "c", -10 )
            ]
    , largestValue = 10
    }


suite : Test
suite =
    describe "Day8"
        [ describe "parse"
            [ test "it works" <|
                \_ ->
                    parse input
                        |> Expect.equalLists parsed
            ]
        , describe "evaluate"
            [ test "it works" <|
                \_ ->
                    evaluate parsed
                        |> Expect.all
                            [ .registers >> Expect.equalDicts evaluated.registers
                            , .largestValue >> Expect.equal evaluated.largestValue
                            ]
            ]
        , describe "largestRegisterValue"
            [ test "it works" <|
                \_ ->
                    largestRegisterValue evaluated
                        |> Expect.equal (Just 1)
            ]
        ]

module TemplateSimple exposing (..)

import Html exposing (Html)


solution1 : String -> Int
solution1 input =
    42


main : Html Never
main =
    Html.div [] [ Html.text (String.fromInt (solution1 puzzleInput)) ]


puzzleInput : String
puzzleInput =
    """
paste input here
"""

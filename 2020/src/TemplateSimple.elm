module TemplateSimple exposing (..)

import Html exposing (Html)


solution1 : String -> Result String Int
solution1 input =
    Ok 42


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
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
paste input here
"""

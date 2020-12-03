module TemplateSandbox exposing (..)

import Browser
import Html exposing (Html)


solution1 : String -> Result String Int
solution1 input =
    Ok 42


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { property : String
    }


init : Model
init =
    { property = "example"
    }


type Msg
    = Msg1


update : Msg -> Model -> Model
update msg model =
    case msg of
        Msg1 ->
            model


view : Model -> Html Msg
view model =
    Html.output []
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

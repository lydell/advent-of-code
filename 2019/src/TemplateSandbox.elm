module TemplateSandbox exposing (..)

import Browser
import Html exposing (Html)


solution1 : String -> Int
solution1 input =
    42


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
    Html.div [] [ Html.text (String.fromInt (solution1 puzzleInput)) ]


puzzleInput : String
puzzleInput =
    """
paste input here
"""

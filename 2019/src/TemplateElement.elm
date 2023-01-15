module TemplateElement exposing (..)

import Browser
import Html exposing (Html)


solution1 : String -> Int
solution1 input =
    42


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { property : String
    }


init : ( Model, Cmd Msg )
init =
    ( { property = "example"
      }
    , Cmd.none
    )


type Msg
    = Msg1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [] [ Html.text (String.fromInt (solution1 puzzleInput)) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


puzzleInput : String
puzzleInput =
    """
paste input here
"""

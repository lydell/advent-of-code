module TemplateElement exposing (..)

import Browser
import Html exposing (Html)


solution1 : String -> Result String Int
solution1 input =
    Ok 42


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


puzzleInput : String
puzzleInput =
    """
paste input here
"""

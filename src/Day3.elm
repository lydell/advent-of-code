module Day3 exposing (..)

import Browser
import Day3Input exposing (puzzleInput)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


type Move
    = Horizontal Int
    | Vertical Int


type alias Wire =
    List Move


parse : String -> ( List Wire, Bounds )
parse input =
    let
        wires =
            parseInput input

        bounds =
            getBounds wires
    in
    ( wires, bounds )


type alias Bounds =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    }


getBounds : List Wire -> Bounds
getBounds wires =
    wires
        |> List.map getWireBounds
        |> combineBounds


getWireBounds : Wire -> Bounds
getWireBounds wire =
    List.foldl
        (\move ( result, currentX, currentY ) ->
            case move of
                Horizontal int ->
                    let
                        x =
                            currentX + int
                    in
                    ( { result | left = min x result.left, right = max x result.right }, x, currentY )

                Vertical int ->
                    let
                        y =
                            currentY + int
                    in
                    ( { result | top = min y result.top, bottom = max y result.bottom }, currentX, y )
        )
        ( { left = 0, right = 0, top = 0, bottom = 0 }, 0, 0 )
        wire
        |> first


first : ( a, b, c ) -> a
first ( a, _, _ ) =
    a


combineBounds : List Bounds -> Bounds
combineBounds boundsList =
    List.foldl
        (\bounds result ->
            { left = min bounds.left result.left
            , right = max bounds.right result.right
            , top = min bounds.top result.top
            , bottom = max bounds.bottom result.bottom
            }
        )
        { left = 0, right = 0, top = 0, bottom = 0 }
        boundsList


parseInput : String -> List Wire
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.map parseLine


parseLine : String -> List Move
parseLine line =
    line
        |> String.split ","
        |> List.filterMap parseMove


parseMove : String -> Maybe Move
parseMove string =
    case String.uncons (String.trim string) of
        Just ( 'U', rest ) ->
            String.toInt rest
                |> Maybe.map (negate >> Vertical)

        Just ( 'D', rest ) ->
            String.toInt rest
                |> Maybe.map Vertical

        Just ( 'L', rest ) ->
            String.toInt rest
                |> Maybe.map (negate >> Horizontal)

        Just ( 'R', rest ) ->
            String.toInt rest
                |> Maybe.map Horizontal

        _ ->
            Debug.todo ("Invalid move: " ++ string)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , parsed : ( List Wire, Bounds )
    }


init : ( Model, Cmd Msg )
init =
    let
        input =
            String.trim puzzleInput
    in
    ( { input = input
      , parsed = parse input
      }
    , Cmd.none
    )


type Msg
    = InputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged text ->
            ( { model | input = text, parsed = parse text }, Cmd.none )


offset : Int
offset =
    10


view : Model -> Html Msg
view model =
    let
        ( wires, bounds ) =
            model.parsed

        width =
            bounds.right - bounds.left + offset * 2

        height =
            bounds.bottom - bounds.top + offset * 2

        viewBox =
            [ bounds.left, bounds.top, width, height ]
                |> List.map String.fromInt
                |> String.join " "

        color index =
            let
                deg =
                    (toFloat index / toFloat (List.length wires) * 360)
                        |> String.fromFloat
            in
            "hsla(" ++ deg ++ "deg, 100%, 50%, 0.5)"

        wiresSvg =
            List.indexedMap (\index wire -> viewWire (color index) wire) wires
    in
    Html.div [ Attr.style "display" "flex", Attr.style "height" "100%" ]
        [ Html.div [ Attr.style "flex" "1", Attr.style "position" "relative" ]
            [ Svg.svg
                [ Attr.style "position" "absolute"
                , Attr.style "top" "0"
                , Attr.style "left" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , SvgAttr.viewBox viewBox
                ]
                [ Svg.g [] wiresSvg
                , Svg.circle
                    [ SvgAttr.cx (String.fromInt offset)
                    , SvgAttr.cy (String.fromInt offset)
                    , SvgAttr.r "1"
                    , SvgAttr.fill "#fff"
                    , Attr.attribute "vector-effect" "non-scaling-size"
                    ]
                    []
                ]
            ]
        , Html.textarea
            [ Attr.style "width" "400px"
            , Attr.style "padding" "5px"
            , Events.onInput InputChanged
            , Attr.value model.input
            ]
            []
        ]


viewWire : String -> Wire -> Svg Msg
viewWire color wire =
    let
        d =
            ("M " ++ String.fromInt offset ++ "," ++ String.fromInt offset)
                :: List.map
                    (\move ->
                        case move of
                            Horizontal int ->
                                "h " ++ String.fromInt int

                            Vertical int ->
                                "v " ++ String.fromInt int
                    )
                    wire
                |> String.join " "
    in
    Svg.path
        [ SvgAttr.d d
        , SvgAttr.fill "none"
        , SvgAttr.stroke color
        , SvgAttr.strokeWidth "2"
        , Attr.attribute "vector-effect" "non-scaling-stroke"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

module Day3 exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Day3Input exposing (puzzleInput)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Task exposing (Task)


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
    , svgViewport : Browser.Dom.Viewport
    }


emptyViewport : Browser.Dom.Viewport
emptyViewport =
    { scene =
        { width = 0
        , height = 0
        }
    , viewport =
        { x = 0
        , y = 0
        , width = 0
        , height = 0
        }
    }


init : ( Model, Cmd Msg )
init =
    let
        input =
            String.trim puzzleInput
    in
    ( { input = input
      , parsed = parse input
      , svgViewport = emptyViewport
      }
    , getSvgViewport
    )


type Msg
    = InputChanged String
    | WindowResized
    | GotSvgViewport (Result Browser.Dom.Error Browser.Dom.Viewport)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged text ->
            ( { model | input = text, parsed = parse text }, Cmd.none )

        WindowResized ->
            ( model, getSvgViewport )

        GotSvgViewport (Ok viewport) ->
            ( { model | svgViewport = viewport }, Cmd.none )

        GotSvgViewport (Err _) ->
            ( model, Cmd.none )


svgId : String
svgId =
    "svg"


getSvgViewport : Cmd Msg
getSvgViewport =
    Browser.Dom.getViewportOf svgId
        |> Task.attempt GotSvgViewport


view : Model -> Html Msg
view model =
    let
        ( wires, bounds ) =
            model.parsed

        width =
            max 1 (bounds.right - bounds.left)

        height =
            max 1 (bounds.bottom - bounds.top)

        svgWidth =
            model.svgViewport.viewport.width

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
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "height" "100%"
        ]
        [ Html.div
            [ Attr.style "flex" "1"
            , Attr.style "position" "relative"
            , Attr.style "margin-right" "30px"
            ]
            [ Svg.svg
                [ Attr.style "position" "absolute"
                , Attr.style "top" "0"
                , Attr.style "left" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "overflow" "visible"
                , SvgAttr.viewBox viewBox
                , SvgAttr.id svgId
                ]
                [ Svg.g [] wiresSvg
                , Svg.path [ SvgAttr.d "M0,0 Z", SvgAttr.stroke "#fff", SvgAttr.strokeWidth "10" ] []
                , Svg.circle
                    [ SvgAttr.cx "0"
                    , SvgAttr.cy "0"
                    , SvgAttr.r (String.fromFloat (8 * toFloat width / svgWidth))
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke "#fff"
                    , SvgAttr.strokeWidth "4"
                    , Attr.attribute "vector-effect" "non-scaling-stroke"
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
            "M 0,0"
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
        , SvgAttr.strokeWidth "4"
        , Attr.attribute "vector-effect" "non-scaling-stroke"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\_ _ -> WindowResized)

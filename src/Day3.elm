module Day3 exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Day3Input exposing (puzzleInput)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events
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
    , boundsElement : Browser.Dom.Element
    , mouse : Maybe ( Float, Float )
    , mouseDown : Maybe ( Float, Float )
    , zoom : Float
    , pan : ( Float, Float )
    }


emptyElement : Browser.Dom.Element
emptyElement =
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
    , element =
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
      , boundsElement = emptyElement
      , mouse = Nothing
      , mouseDown = Nothing
      , zoom = 1
      , pan = ( 0, 0 )
      }
    , getBoundsElement
    )


type Msg
    = InputChanged String
    | WindowResized
    | GotBoundsElement (Result Browser.Dom.Error Browser.Dom.Element)
    | BoundsMouseMove ( Float, Float )
    | BoundsMouseOut
    | MouseDown ( Float, Float )
    | MouseUp
    | MouseMove ( Float, Float )
    | Wheel ( Float, ( Float, Float ) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged text ->
            ( { model
                | input = text
                , parsed = parse text
                , zoom = 1
                , pan = ( 0, 0 )
              }
            , getBoundsElement
            )

        WindowResized ->
            ( model, getBoundsElement )

        GotBoundsElement (Ok element) ->
            ( { model | boundsElement = element }, Cmd.none )

        GotBoundsElement (Err _) ->
            ( model, Cmd.none )

        BoundsMouseMove ( x, y ) ->
            ( { model | mouse = Just ( x, y ) }, Cmd.none )

        BoundsMouseOut ->
            ( { model | mouse = Nothing }, Cmd.none )

        MouseDown ( x, y ) ->
            let
                ( panX, panY ) =
                    model.pan
            in
            ( { model | mouseDown = Just ( panX + x, panY + y ) }, Cmd.none )

        MouseUp ->
            ( { model | mouseDown = Nothing }, Cmd.none )

        MouseMove ( x, y ) ->
            case model.mouseDown of
                Just ( startX, startY ) ->
                    let
                        dx =
                            startX - x

                        dy =
                            startY - y
                    in
                    ( { model | pan = ( dx, dy ) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Wheel ( delta, ( x, y ) ) ->
            let
                zoom =
                    clamp 0.5 100 (model.zoom + delta / 100)

                zoomDelta =
                    zoom - model.zoom

                ( panX, panY ) =
                    model.pan

                ( posX, posY ) =
                    ( x - model.boundsElement.element.x
                    , y - model.boundsElement.element.y
                    )

                pan =
                    ( panX + zoomDelta * posX
                    , panY + zoomDelta * posY
                    )
            in
            ( { model
                | zoom = zoom
                , pan = pan
              }
            , Cmd.none
            )


boundsId : String
boundsId =
    "bounds"


getBoundsElement : Cmd Msg
getBoundsElement =
    Browser.Dom.getElement boundsId
        |> Task.attempt GotBoundsElement


view : Model -> Html Msg
view model =
    let
        ( wires, bounds ) =
            model.parsed

        width =
            max 1 (toFloat (bounds.right - bounds.left) / model.zoom)

        height =
            max 1 (toFloat (bounds.bottom - bounds.top) / model.zoom)

        boundsWidth =
            max 1 model.boundsElement.element.width

        boundsHeight =
            max 1 model.boundsElement.element.height

        ( panX, panY ) =
            model.pan

        left =
            toFloat bounds.left + panX / boundsWidth * width

        top =
            toFloat bounds.top + panY / boundsHeight * height

        mouse =
            Maybe.map
                (\( x, y ) ->
                    ( round (left + (x - model.boundsElement.element.x) / boundsWidth * width)
                    , round (top + (y - model.boundsElement.element.y) / boundsHeight * height)
                    )
                )
                model.mouse

        viewBox =
            [ left
            , top
            , max 1 width
            , max 1 height
            ]
                |> List.map String.fromFloat
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
        , Attr.style "cursor"
            (case model.mouseDown of
                Just _ ->
                    "grabbing"

                Nothing ->
                    "default"
            )
        , Html.Events.on "mousedown" (Json.Decode.map MouseDown (skipTextarea mousePositionDecoder))
        , Html.Events.onMouseUp MouseUp
        , Html.Events.on "mousemove" (Json.Decode.map MouseMove mousePositionDecoder)
        , Html.Events.on "wheel" (Json.Decode.map Wheel (skipTextarea wheelDecoder))
        ]
        [ Html.div
            [ Attr.style "flex" "1"
            , Attr.style "position" "relative"
            , Attr.style "margin-right" "30px"
            ]
            [ case mouse of
                Just ( x, y ) ->
                    Html.text (String.fromInt (abs x + abs y))

                Nothing ->
                    Html.text ""
            , Svg.svg
                [ Attr.style "position" "absolute"
                , Attr.style "top" "0"
                , Attr.style "left" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "overflow" "visible"
                , SvgAttr.viewBox viewBox
                ]
                [ Svg.g [] wiresSvg
                , Svg.circle
                    [ SvgAttr.cx "0"
                    , SvgAttr.cy "0"
                    , SvgAttr.r (String.fromFloat (8 * width / boundsWidth))
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke "#fff"
                    , SvgAttr.strokeWidth "4"
                    , Attr.attribute "vector-effect" "non-scaling-stroke"
                    ]
                    []
                , case mouse of
                    Just ( x, y ) ->
                        Svg.circle
                            [ SvgAttr.cx (String.fromInt x)
                            , SvgAttr.cy (String.fromInt y)
                            , SvgAttr.r (String.fromFloat (8 * width / boundsWidth))
                            , SvgAttr.fill "none"
                            , SvgAttr.stroke "#fff"
                            , SvgAttr.strokeWidth "4"
                            , Attr.attribute "vector-effect" "non-scaling-stroke"
                            ]
                            []

                    Nothing ->
                        Svg.text ""
                , Svg.rect
                    [ SvgAttr.x (String.fromFloat left)
                    , SvgAttr.y (String.fromFloat top)
                    , SvgAttr.width (String.fromFloat width)
                    , SvgAttr.height (String.fromFloat height)

                    --, SvgAttr.fill "transparent"
                    , SvgAttr.fill "rgba(155, 0, 0, 0.1)"
                    , SvgAttr.id boundsId
                    , Svg.Events.on "mousemove" (Json.Decode.map BoundsMouseMove mousePositionDecoder)
                    , Svg.Events.onMouseOut BoundsMouseOut
                    ]
                    []
                ]
            ]
        , Html.textarea
            [ Attr.style "width" "400px"
            , Attr.style "padding" "5px"
            , Attr.style "position" "relative"
            , Attr.style "z-index" "1"
            , Html.Events.onInput InputChanged
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


skipTextarea : Json.Decode.Decoder a -> Json.Decode.Decoder a
skipTextarea decoder =
    Json.Decode.at [ "target", "nodeName" ] Json.Decode.string
        |> Json.Decode.andThen
            (\nodeName ->
                case nodeName of
                    "TEXTAREA" ->
                        Json.Decode.fail "Ignored"

                    _ ->
                        decoder
            )


mousePositionDecoder : Json.Decode.Decoder ( Float, Float )
mousePositionDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


wheelDecoder : Json.Decode.Decoder ( Float, ( Float, Float ) )
wheelDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "deltaY" Json.Decode.float)
        mousePositionDecoder


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\_ _ -> WindowResized)

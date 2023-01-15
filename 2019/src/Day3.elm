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
import Svg.Keyed
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
            Nothing


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
    , boundsElement : Maybe Browser.Dom.Element
    , mouse : Maybe ( Float, Float )
    , mouseDown : Maybe ( Float, Float )
    , zoom : Float
    , pan : ( Float, Float )
    }


init : ( Model, Cmd Msg )
init =
    let
        input =
            String.trim puzzleInput
    in
    ( { input = input
      , parsed = parse input
      , boundsElement = Nothing
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
    | MouseDown ( Float, Float )
    | MouseUp ( Float, Float )
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
            ( { model | boundsElement = Just element }, Cmd.none )

        GotBoundsElement (Err error) ->
            ( { model | boundsElement = Nothing }, Cmd.none )

        MouseDown ( x, y ) ->
            case model.boundsElement of
                Just boundsElement ->
                    let
                        ( panX, panY ) =
                            model.pan

                        dx =
                            panX + x / boundsElement.element.width / model.zoom

                        dy =
                            panY + y / boundsElement.element.height / model.zoom
                    in
                    ( { model | mouseDown = Just ( dx, dy ) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        MouseUp ( x, y ) ->
            ( { model | mouse = Just ( x, y ), mouseDown = Nothing }, Cmd.none )

        MouseMove ( x, y ) ->
            case model.mouseDown of
                Just ( startX, startY ) ->
                    case model.boundsElement of
                        Just boundsElement ->
                            let
                                dx =
                                    startX - x / boundsElement.element.width / model.zoom

                                dy =
                                    startY - y / boundsElement.element.height / model.zoom
                            in
                            ( { model | mouse = Nothing, pan = ( dx, dy ) }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( { model | mouse = Just ( x, y ) }, Cmd.none )

        Wheel ( delta, ( x, y ) ) ->
            case model.boundsElement of
                Just boundsElement ->
                    let
                        zoom =
                            clamp minZoom maxZoom (model.zoom - delta / 100)

                        ( panX, panY ) =
                            model.pan

                        ( posX, posY ) =
                            ( (x - boundsElement.element.x) / boundsElement.element.width
                            , (y - boundsElement.element.y) / boundsElement.element.height
                            )

                        pan =
                            ( panX + posX / model.zoom - posX / zoom
                            , panY + posY / model.zoom - posY / zoom
                            )
                    in
                    ( { model
                        | zoom = zoom
                        , pan = pan
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


minZoom : Float
minZoom =
    0.5


maxZoom : Float
maxZoom =
    100


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
        mouseBasedAttrs =
            case model.mouseDown of
                Just _ ->
                    [ Attr.style "cursor" "grabbing"
                    , Html.Events.on "mouseup"
                        (Json.Decode.map MouseUp mousePositionDecoder)
                    ]

                Nothing ->
                    [ Attr.style "cursor" "default"
                    , Html.Events.on "mousedown"
                        (Json.Decode.map MouseDown (skipTextarea mousePositionDecoder))
                    , Html.Events.preventDefaultOn "wheel"
                        (Json.Decode.map (\data -> ( Wheel data, True )) (skipTextarea wheelDecoder))
                    ]
    in
    Html.div
        ([ Attr.style "display" "flex"
         , Attr.style "height" "100%"
         , Html.Events.on "mousemove"
            (Json.Decode.map MouseMove mousePositionDecoder)
         ]
            ++ mouseBasedAttrs
        )
        [ Html.div
            [ Attr.style "flex" "1"
            , Attr.style "position" "relative"
            , Attr.style "margin-right" "30px"
            ]
            (case model.boundsElement of
                Just boundsElement ->
                    viewSvg boundsElement model

                Nothing ->
                    -- View an empty SVG element so we can measure it.
                    [ viewSvgElement (getViewBox (Tuple.second model.parsed) model.pan model.zoom) [] ]
            )
        , Html.textarea
            [ Attr.style "width" "400px"
            , Attr.style "padding" "0.5em"
            , Attr.style "position" "relative"
            , Attr.style "z-index" "1"
            , Attr.style "resize" "none"
            , Attr.style "border" "none"
            , Attr.style "border-radius" "0"
            , Html.Events.onInput InputChanged
            , Attr.value model.input
            ]
            []
        ]


viewSvg : Browser.Dom.Element -> Model -> List (Html msg)
viewSvg boundsElement model =
    let
        ( wires, bounds ) =
            model.parsed

        boundsWidth =
            max 1 boundsElement.element.width

        boundsHeight =
            boundsElement.element.height

        viewBox =
            getViewBox bounds model.pan model.zoom

        mouse =
            Maybe.map
                (\( x, y ) ->
                    ( round (viewBox.left + (x - boundsElement.element.x) / boundsWidth * viewBox.width)
                    , round (viewBox.top + (y - boundsElement.element.y) / boundsHeight * viewBox.height)
                    )
                )
                model.mouse

        color index =
            let
                deg =
                    (toFloat index / toFloat (List.length wires) * 360)
                        |> String.fromFloat
            in
            "hsl(" ++ deg ++ "deg, 100%, 50%)"

        pxPerUnit =
            -- Only animate when not zoomed to avoid more animations when zooming in.
            if model.zoom == 1 then
                Just (boundsWidth / viewBox.width)

            else
                Nothing

        wiresSvg =
            List.indexedMap (\index wire -> viewWire (color index) pxPerUnit wire) wires
    in
    [ case mouse of
        Just ( x, y ) ->
            Html.span
                [ Attr.style "position" "relative"
                , Attr.style "z-index" "1"
                , Attr.style "background-color" "#0f0f23"
                ]
                [ Html.text (String.fromInt (abs x + abs y)) ]

        Nothing ->
            Html.text ""
    , viewSvgElement viewBox
        [ Svg.Keyed.node "g" [] wiresSvg
        , Svg.circle
            [ SvgAttr.cx "0"
            , SvgAttr.cy "0"
            , SvgAttr.r (String.fromFloat (8 * viewBox.width / boundsWidth))
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
                    , SvgAttr.r (String.fromFloat (8 * viewBox.width / boundsWidth))
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke "#fff"
                    , SvgAttr.strokeWidth "4"
                    , Attr.attribute "vector-effect" "non-scaling-stroke"
                    ]
                    []

            Nothing ->
                Svg.text ""
        ]
    ]


getViewBox : Bounds -> ( Float, Float ) -> Float -> ViewBox
getViewBox bounds ( panX, panY ) zoom =
    let
        width =
            max (1 / maxZoom) (toFloat (bounds.right - bounds.left) / zoom)

        height =
            max (1 / maxZoom) (toFloat (bounds.bottom - bounds.top) / zoom)

        left =
            toFloat bounds.left + panX * width * zoom

        top =
            toFloat bounds.top + panY * height * zoom
    in
    { left = left, top = top, width = width, height = height }


type alias ViewBox =
    { left : Float
    , top : Float
    , width : Float
    , height : Float
    }


viewSvgElement : ViewBox -> List (Svg msg) -> Svg msg
viewSvgElement viewBox children =
    let
        viewBoxAttr =
            [ viewBox.left, viewBox.top, viewBox.width, viewBox.height ]
                |> List.map String.fromFloat
                |> String.join " "
    in
    Svg.svg
        [ Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "overflow" "visible"
        , SvgAttr.viewBox viewBoxAttr
        ]
        (children
            ++ [ Svg.rect
                    [ SvgAttr.x (String.fromFloat viewBox.left)
                    , SvgAttr.y (String.fromFloat viewBox.top)
                    , SvgAttr.width (String.fromFloat viewBox.width)
                    , SvgAttr.height (String.fromFloat viewBox.height)
                    , SvgAttr.fill "transparent"
                    , SvgAttr.id boundsId
                    ]
                    []
               ]
        )


viewWire : String -> Maybe Float -> Wire -> ( String, Svg msg )
viewWire color maybePxPerUnit wire =
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

        animation =
            case maybePxPerUnit of
                Just pxPerUnit ->
                    let
                        length =
                            wire
                                |> List.map
                                    (\move ->
                                        case move of
                                            Horizontal int ->
                                                abs int

                                            Vertical int ->
                                                abs int
                                    )
                                |> List.sum
                                |> toFloat
                                |> (*) pxPerUnit

                        duration =
                            length / 2000
                    in
                    [ Attr.style "stroke-dasharray" (String.fromFloat length)
                    , Attr.style "stroke-dashoffset" (String.fromFloat length)
                    , Attr.style "animation" ("draw " ++ String.fromFloat duration ++ "s linear forwards")
                    ]

                Nothing ->
                    []
    in
    ( d
    , Svg.path
        ([ SvgAttr.d d
         , SvgAttr.fill "none"
         , SvgAttr.stroke color
         , SvgAttr.strokeWidth "4"
         , Attr.attribute "vector-effect" "non-scaling-stroke"
         , SvgAttr.strokeLinecap "round"
         , SvgAttr.strokeLinejoin "round"
         ]
            ++ animation
        )
        []
    )


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

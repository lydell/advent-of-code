module AB exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Input
import Process
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Keyed
import Task
import Time


sandStart : ( Int, Int )
sandStart =
    ( 500, 0 )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { walls : List (List ( Int, Int ))
    , grid : Dict ( Int, Int ) Item
    , bounds : Bounds
    , currentSand : ( Int, Int )
    , playing : Bool
    , steppingToEnd : Bool
    , part : Part
    , fps : Int
    , fpsInput : String
    , input : String
    , inputError : Maybe String
    }


type Item
    = Wall
    | Sand


type Part
    = A
    | B


type Msg
    = Step
    | PlayPauseToggled
    | StepToEndClicked
    | StepToEndStarted
    | SwitchPartClicked Part
    | FpsInputChanged String
    | InputChanged String


init : () -> ( Model, Cmd Msg )
init () =
    let
        fps =
            60
    in
    ( initPart
        { walls = []
        , grid = Dict.empty
        , bounds = wallsToBounds []
        , currentSand = sandStart
        , playing = False
        , steppingToEnd = False
        , part = A
        , fps = fps
        , fpsInput = String.fromInt fps
        , input = Input.input
        , inputError = Nothing
        }
    , Cmd.none
    )


initPart : Model -> Model
initPart model =
    case parse model.input of
        Ok walls ->
            let
                bounds =
                    wallsToBounds walls

                adjustedBounds =
                    case model.part of
                        A ->
                            bounds

                        B ->
                            adjustBoundsForPartB bounds
            in
            { model
                | walls = walls
                , grid = wallsToGrid walls
                , bounds = adjustedBounds
                , currentSand = sandStart
                , inputError = Nothing
            }

        Err errors ->
            { model | inputError = Just (String.join "\n" errors) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            ( step model, Cmd.none )

        PlayPauseToggled ->
            ( { model | playing = not model.playing }, Cmd.none )

        StepToEndClicked ->
            ( { model | steppingToEnd = True, playing = False }
            , Process.sleep 50 |> Task.perform (always StepToEndStarted)
            )

        StepToEndStarted ->
            let
                nextModel =
                    stepToEnd { model | playing = True }
            in
            ( { nextModel | steppingToEnd = False }, Cmd.none )

        SwitchPartClicked part ->
            ( initPart { model | part = part }, Cmd.none )

        FpsInputChanged input ->
            ( { model
                | fpsInput = input
                , fps =
                    case String.toInt input of
                        Just fps ->
                            max fpsMin fps

                        Nothing ->
                            model.fps
              }
            , Cmd.none
            )

        InputChanged input ->
            ( initPart
                { model
                    | input =
                        if String.isEmpty (String.trim input) then
                            Input.input

                        else
                            input
                }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing then
        Time.every (1000 / toFloat model.fps) (always Step)

    else
        Sub.none


parse : String -> Result (List String) (List (List ( Int, Int )))
parse input =
    input
        |> String.trim
        |> String.lines
        |> List.indexedMap
            (\lineIndex line ->
                line
                    |> String.split " -> "
                    |> List.indexedMap
                        (\segmentIndex segment ->
                            case String.split "," segment of
                                [ a, b ] ->
                                    case ( String.toInt a, String.toInt b ) of
                                        ( Just aInt, Just bInt ) ->
                                            Ok ( aInt, bInt )

                                        _ ->
                                            Err ("Line " ++ String.fromInt (lineIndex + 1) ++ ", segment " ++ String.fromInt (segmentIndex + 1) ++ ": Invalid ints: " ++ segment)

                                _ ->
                                    Err ("Line " ++ String.fromInt (lineIndex + 1) ++ ", segment " ++ String.fromInt (segmentIndex + 1) ++ ": Expected one comma, but got: " ++ segment)
                        )
                    |> collectResults
            )
        |> collectResults
        |> Result.mapError List.concat


collectResults : List (Result x a) -> Result (List x) (List a)
collectResults list =
    let
        oks =
            List.filterMap Result.toMaybe list

        errors =
            List.filterMap
                (\result ->
                    case result of
                        Err error ->
                            Just error

                        Ok _ ->
                            Nothing
                )
                list
    in
    if List.isEmpty errors then
        Ok oks

    else
        Err errors


wallsToGrid : List (List ( Int, Int )) -> Dict ( Int, Int ) Item
wallsToGrid =
    List.concatMap lineToCoordinates
        >> Dict.fromList


lineToCoordinates : List ( Int, Int ) -> List ( ( Int, Int ), Item )
lineToCoordinates line =
    case line of
        ( x1, y1 ) :: ( x2, y2 ) :: rest ->
            let
                coordinates =
                    if x1 == x2 then
                        range y1 y2
                            |> List.map (\y -> ( ( x1, y ), Wall ))

                    else
                        range x1 x2
                            |> List.map (\x -> ( ( x, y1 ), Wall ))
            in
            coordinates ++ lineToCoordinates (( x2, y2 ) :: rest)

        _ ->
            []


range : Int -> Int -> List Int
range a b =
    List.range (min a b) (max a b)


type alias Bounds =
    { xMin : Int
    , xMax : Int
    , yMin : Int
    , yMax : Int
    }


wallsToBounds : List (List ( Int, Int )) -> Bounds
wallsToBounds walls =
    let
        ( xs, ys ) =
            ([ sandStart ] :: walls)
                |> List.concat
                |> List.unzip
    in
    { xMin = minimum xs
    , xMax = maximum xs
    , yMin = minimum ys
    , yMax = maximum ys
    }


adjustBoundsForPartB : Bounds -> Bounds
adjustBoundsForPartB bounds =
    let
        height =
            bounds.yMax + 1 - bounds.yMin

        ( x, _ ) =
            sandStart

        xMin =
            x - height

        xMax =
            x + height
    in
    { xMin = min xMin bounds.xMin
    , xMax = max xMax bounds.xMax
    , yMin = bounds.yMin
    , yMax = bounds.yMax + 1
    }


minimum : List Int -> Int
minimum =
    List.minimum >> Maybe.withDefault 0


maximum : List Int -> Int
maximum =
    List.maximum >> Maybe.withDefault 0


countSand : Dict ( Int, Int ) Item -> Int
countSand =
    Dict.values >> List.filter ((==) Sand) >> List.length


step : Model -> Model
step model =
    let
        ( x, y ) =
            model.currentSand
    in
    case Dict.get ( x, y + 1 ) model.grid of
        Nothing ->
            if y >= model.bounds.yMax then
                case model.part of
                    A ->
                        { model
                            | currentSand = sandStart
                            , playing = False
                        }

                    B ->
                        { model
                            | currentSand = sandStart
                            , grid = Dict.insert ( x, y ) Sand model.grid
                        }

            else
                { model | currentSand = ( x, y + 1 ) }

        Just _ ->
            case Dict.get ( x - 1, y + 1 ) model.grid of
                Nothing ->
                    { model | currentSand = ( x - 1, y + 1 ) }

                Just _ ->
                    case Dict.get ( x + 1, y + 1 ) model.grid of
                        Nothing ->
                            { model | currentSand = ( x + 1, y + 1 ) }

                        Just _ ->
                            { model
                                | currentSand = sandStart
                                , grid = Dict.insert ( x, y ) Sand model.grid
                                , playing =
                                    if ( x, y ) == sandStart then
                                        False

                                    else
                                        model.playing
                            }


stepToEnd : Model -> Model
stepToEnd model =
    let
        nextModel =
            step model
    in
    if nextModel.playing then
        stepToEnd nextModel

    else
        nextModel


fpsMin : Int
fpsMin =
    1


view : Model -> Html Msg
view model =
    Html.div []
        [ viewSvg model
        , viewControls model
        ]


viewControls : Model -> Html Msg
viewControls model =
    Html.div [ Html.Attributes.id "controls-wrapper" ]
        [ Html.div [ Html.Attributes.id "controls" ]
            [ Html.button [ Html.Events.onClick PlayPauseToggled ]
                [ Html.text
                    (if model.playing then
                        "⏸"

                     else
                        "▶️"
                    )
                ]
            , Html.button [ Html.Events.onClick Step ]
                [ Html.text "⏩" ]
            , if model.steppingToEnd then
                Html.button []
                    [ Html.text "⏳" ]

              else
                Html.button [ Html.Events.onClick StepToEndClicked ]
                    [ Html.text "⏭" ]
            , Html.button [ Html.Events.onClick (SwitchPartClicked model.part) ]
                [ Html.text "↩️" ]
            , Html.label []
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.name "part"
                    , Html.Events.onCheck (always (SwitchPartClicked A))
                    , Html.Attributes.checked (model.part == A)
                    ]
                    []
                , Html.text "Part 1"
                ]
            , Html.label []
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.name "part"
                    , Html.Events.onCheck (always (SwitchPartClicked B))
                    , Html.Attributes.checked (model.part == B)
                    ]
                    []
                , Html.text "Part 2"
                ]
            , Html.label []
                [ Html.text "FPS: "
                , Html.input
                    [ Html.Attributes.type_ "number"
                    , Html.Attributes.min (String.fromInt fpsMin)
                    , Html.Attributes.value model.fpsInput
                    , Html.Events.onInput FpsInputChanged
                    , Html.Attributes.style "width" "4em"
                    ]
                    []
                ]
            ]
        , Html.div [] [ Html.text ("Sand count: " ++ String.fromInt (countSand model.grid)) ]
        , Html.textarea
            [ Html.Attributes.placeholder "Paste input here"
            , Html.Attributes.style "width" "11em"
            , Html.Events.onInput InputChanged
            ]
            []
        , case model.inputError of
            Just error ->
                Html.pre [] [ Html.text error ]

            Nothing ->
                Html.text ""
        ]


viewSvg : Model -> Html Msg
viewSvg model =
    let
        { bounds } =
            model

        padding =
            1

        viewBox =
            [ bounds.xMin - padding
            , bounds.yMin - padding
            , bounds.xMax - bounds.xMin + 2 * padding
            , bounds.yMax - bounds.yMin + 2 * padding
            ]
                |> List.map String.fromInt
                |> String.join " "
                |> Svg.Attributes.viewBox
    in
    Svg.Keyed.node "svg"
        [ viewBox ]
        [ ( "viewWalls", viewWalls model.walls )
        , ( "viewSandStart", viewSand [ Svg.Attributes.opacity "0.5" ] sandStart )
        , ( "viewCurrentSand" ++ String.fromInt (Dict.size model.grid)
          , viewCurrentSand model.fps model.currentSand
          )
        , ( "viewStationarySand", viewStationarySand model.grid )
        ]


viewWalls : List (List ( Int, Int )) -> Svg msg
viewWalls walls =
    let
        d =
            walls
                |> List.concatMap
                    (List.indexedMap
                        (\index ( x, y ) ->
                            if index == 0 then
                                "M" ++ String.fromInt x ++ "," ++ String.fromInt y

                            else
                                "L" ++ String.fromInt x ++ "," ++ String.fromInt y
                        )
                    )
                |> String.join " "
    in
    Svg.path
        [ Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeWidth "1"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        , Svg.Attributes.d d
        ]
        []


viewSand : List (Svg.Attribute msg) -> ( Int, Int ) -> Svg msg
viewSand attributes ( x, y ) =
    Svg.circle
        ([ Svg.Attributes.cx (String.fromInt x)
         , Svg.Attributes.cy (String.fromInt y)
         , Svg.Attributes.r "0.5"
         , Svg.Attributes.fill "tan"
         ]
            ++ attributes
        )
        []


viewCurrentSand : Int -> ( Int, Int ) -> Svg msg
viewCurrentSand fps ( x, y ) =
    viewSand
        [ Svg.Attributes.transform ("translate(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")")
        , Svg.Attributes.style ("transition: transform " ++ String.fromFloat (1000 / toFloat fps) ++ "ms linear")
        ]
        ( 0, 0 )


viewStationarySand : Dict ( Int, Int ) Item -> Svg msg
viewStationarySand =
    Dict.filter (\_ item -> item == Sand)
        >> Dict.keys
        >> List.map (viewSand [])
        >> Svg.g []

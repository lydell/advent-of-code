module AB exposing (main)

import Array exposing (Array)
import Bitwise
import Browser
import Browser.Dom
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Input
import Process
import Regex exposing (Regex)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Keyed
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { gas : Array Push
    , gasIndex : Int
    , rockCounter : Int
    , current : Maybe PositionedRock
    , stopped : List PositionedRock
    , occupied : Array Int -- 1 array item per row, 1 bit per column
    , counts : Dict Int Int -- number of rocks up to and including the row
    , repeat : Maybe RepeatData
    , chamberHeight : Int
    , movement : Movement
    , scrolling : Bool
    , playing : Bool
    , steppingToEnd : Bool
    , part : Part
    , fps : Int
    , fpsInput : String
    , gasInput : String
    , gasInputError : Maybe String
    , limitInput : String
    }


type Push
    = Left
    | Right


type alias Rock =
    { bits : List Int
    , variant : Variant
    , width : Int
    , height : Int
    }


type Variant
    = Minus
    | Plus
    | J
    | I
    | Square


type alias PositionedRock =
    { rock : Rock
    , x : Int
    , y : Int
    }


type alias RepeatData =
    { repeat : Repeat
    , repeatsToSkip : Int
    }


type Movement
    = MoveHorizontally
    | MoveDown


type Part
    = ExampleA
    | ExampleB
    | FullA
    | FullB
    | Custom { input : String, limit : Int }


type Msg
    = NextStepClicked
    | TimePassed
    | PlayPauseToggled
    | StepToEndClicked
    | StepToEndStarted
    | SwitchPartClicked Part
    | SwitchToCustomClicked
    | FpsInputChanged String
    | GasInputChanged String
    | LimitInputChanged String
    | Scrolled


init : () -> ( Model, Cmd Msg )
init () =
    let
        fps =
            8
    in
    initPart
        { gas = Array.empty
        , gasIndex = 0
        , rockCounter = 0
        , current = Nothing
        , stopped = []
        , occupied = Array.empty
        , counts = Dict.empty
        , repeat = Nothing
        , chamberHeight = 0
        , movement = MoveHorizontally
        , scrolling = False
        , playing = False
        , steppingToEnd = False
        , part = ExampleA
        , fps = fps
        , fpsInput = String.fromInt fps
        , gasInput = "<>"
        , gasInputError = Nothing
        , limitInput = "2022"
        }


initPart : Model -> ( Model, Cmd Msg )
initPart model =
    case parse (partToInput model.part) of
        Ok gas ->
            { model
                | gas = gas
                , gasIndex = 0
                , rockCounter = 0
                , current = Nothing
                , stopped = []

                -- Add the bottom edge as occupied. to avoid having to check for it.
                , occupied = Array.fromList [ 2 ^ chamberWidth - 1 ]
                , counts = Dict.singleton 0 0
                , repeat = Nothing
                , chamberHeight = viewRemaining model.fps 0 0 (partToLimit model.part) |> Tuple.second
                , movement = MoveHorizontally
                , gasInputError = Nothing
            }
                |> step
                |> scrollToBottom

        Err errors ->
            ( { model | gasInputError = Just (String.join "\n" errors) }, Cmd.none )


partToLimit : Part -> Int
partToLimit part =
    case part of
        ExampleA ->
            2022

        ExampleB ->
            1000000000000

        FullA ->
            2022

        FullB ->
            1000000000000

        Custom { limit } ->
            limit


partToInput : Part -> String
partToInput part =
    case part of
        ExampleA ->
            Input.example

        ExampleB ->
            Input.example

        FullA ->
            Input.input

        FullB ->
            Input.input

        Custom { input } ->
            input


oneBasedIntToRock : Int -> Rock
oneBasedIntToRock i =
    let
        variant =
            intToVariant (i - 1)

        ( bits, width ) =
            variantToReversedBits variant
    in
    { bits = bits
    , variant = variant
    , width = width
    , height = List.length bits
    }


intToVariant : Int -> Variant
intToVariant i =
    case i |> modBy 5 of
        0 ->
            Minus

        1 ->
            Plus

        2 ->
            J

        3 ->
            I

        _ ->
            Square


{-| <https://colorbrewer2.org/?type=qualitative&scheme=Set3&n=5>
-}
variantToColor : Variant -> String
variantToColor variant =
    case variant of
        Minus ->
            "#8dd3c7"

        Plus ->
            "#ffffb3"

        J ->
            "#bebada"

        I ->
            "#fb8072"

        Square ->
            "#80b1d3"


variantToReversedBits : Variant -> ( List Int, Int )
variantToReversedBits variant =
    case variant of
        Minus ->
            ( [ 15 -- 0b1111
              ]
            , 4
            )

        Plus ->
            ( [ 2 -- 0b010
              , 7 -- 0b111
              , 2 -- 0b010
              ]
            , 3
            )

        J ->
            ( [ 7 -- 0b111
              , 1 -- 0b001
              , 1 -- 0b001
              ]
            , 3
            )

        I ->
            ( [ 1 -- 0b1
              , 1 -- 0b1
              , 1 -- 0b1
              , 1 -- 0b1
              ]
            , 1
            )

        Square ->
            ( [ 3 -- 0b11
              , 3 -- 0b11
              ]
            , 2
            )


scrollToBottom : Model -> ( Model, Cmd Msg )
scrollToBottom model =
    if model.scrolling then
        ( model, Cmd.none )

    else
        Browser.Dom.getViewport
            |> Task.andThen
                (\viewport ->
                    Browser.Dom.setViewport 0 (viewport.scene.height - viewport.viewport.height)
                )
            |> Task.perform (\() -> Scrolled)
            |> Tuple.pair { model | scrolling = True }


currentSvgId : String
currentSvgId =
    "currentSvgId"


scroll : Model -> ( Model, Cmd Msg )
scroll model =
    if model.scrolling then
        ( model, Cmd.none )

    else
        Browser.Dom.getElement currentSvgId
            |> Task.andThen
                (\{ element, viewport } ->
                    let
                        topDiff =
                            element.y - viewport.y
                    in
                    if topDiff < 0 then
                        Browser.Dom.setViewport viewport.x (viewport.y + topDiff - viewport.height / 2)
                            |> Task.andThen
                                (\() ->
                                    Process.sleep 200
                                )

                    else
                        Task.succeed ()
                )
            |> Task.attempt (\_ -> Scrolled)
            |> Tuple.pair { model | scrolling = True }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStepClicked ->
            scroll (step model)

        TimePassed ->
            let
                nextModel =
                    step model

                finalModel =
                    if model.current == nextModel.current && nextModel.playing then
                        step nextModel

                    else
                        nextModel
            in
            scroll finalModel

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
            initPart { model | part = part }

        SwitchToCustomClicked ->
            initPart
                { model
                    | part =
                        Custom
                            { input = model.gasInput
                            , limit =
                                case String.toInt model.limitInput of
                                    Just limit ->
                                        max limitMin limit

                                    Nothing ->
                                        2022
                            }
                }

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

        GasInputChanged input ->
            case model.part of
                Custom data ->
                    initPart
                        { model
                            | part = Custom { data | input = input }
                            , gasInput = input
                        }

                _ ->
                    ( model, Cmd.none )

        LimitInputChanged input ->
            case model.part of
                Custom data ->
                    initPart
                        { model
                            | part =
                                Custom
                                    { data
                                        | limit =
                                            case String.toInt input of
                                                Just limit ->
                                                    max limitMin limit

                                                Nothing ->
                                                    data.limit
                                    }
                            , limitInput = input
                        }

                _ ->
                    ( model, Cmd.none )

        Scrolled ->
            ( { model | scrolling = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing then
        Time.every (1000 / toFloat model.fps) (always TimePassed)

    else
        Sub.none


parse : String -> Result (List String) (Array Push)
parse input =
    input
        |> String.toList
        |> List.filter (not << isWhitespace)
        |> List.map
            (\char ->
                case char of
                    '<' ->
                        Ok Left

                    '>' ->
                        Ok Right

                    _ ->
                        Err ("Invalid character: " ++ String.fromChar char)
            )
        |> collectResults
        |> Result.map Array.fromList


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\t' || char == '\n' || char == '\u{000D}'


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


step : Model -> Model
step model =
    case model.current of
        Nothing ->
            if model.rockCounter >= partToLimit model.part then
                { model | playing = False }

            else
                let
                    nextRockCounter =
                        model.rockCounter + 1
                in
                { model
                    | rockCounter = nextRockCounter
                    , current =
                        Just
                            { rock = oneBasedIntToRock nextRockCounter
                            , x = startLeft
                            , y = getStoppedY model.stopped + startBottom
                            }
                }

        Just current ->
            case model.movement of
                MoveHorizontally ->
                    let
                        movedHorizontally =
                            case Array.get model.gasIndex model.gas of
                                Nothing ->
                                    current

                                Just push ->
                                    case push of
                                        Left ->
                                            if current.x == 0 then
                                                current

                                            else
                                                let
                                                    next =
                                                        { current | x = current.x - 1 }
                                                in
                                                if overlaps model.occupied next then
                                                    current

                                                else
                                                    next

                                        Right ->
                                            if current.x + current.rock.width == chamberWidth then
                                                current

                                            else
                                                let
                                                    next =
                                                        { current | x = current.x + 1 }
                                                in
                                                if overlaps model.occupied next then
                                                    current

                                                else
                                                    next
                    in
                    { model
                        | gasIndex = model.gasIndex + 1 |> modBy (Array.length model.gas)
                        , current = Just movedHorizontally
                        , movement = MoveDown
                    }

                MoveDown ->
                    let
                        movedDown =
                            { current | y = current.y - 1 }
                    in
                    if overlaps model.occupied movedDown then
                        let
                            ( newOccupied, newCounts ) =
                                occupy model.occupied model.counts current
                        in
                        if model.rockCounter >= partToLimit model.part then
                            { model
                                | current = Nothing
                                , stopped = current :: model.stopped
                                , occupied = newOccupied
                                , counts = newCounts
                                , playing = False
                            }

                        else
                            let
                                newStopped =
                                    current :: model.stopped

                                nextRockCounter =
                                    model.rockCounter + 1

                                nextRock =
                                    oneBasedIntToRock nextRockCounter
                            in
                            findAndUpdateRepeat
                                { model
                                    | rockCounter = nextRockCounter
                                    , current =
                                        Just
                                            { rock = nextRock
                                            , x = startLeft
                                            , y = getStoppedY newStopped + startBottom + nextRock.height
                                            }
                                    , stopped = newStopped
                                    , occupied = newOccupied
                                    , counts = newCounts
                                    , movement = MoveHorizontally
                                }

                    else
                        { model
                            | current = Just movedDown
                            , movement = MoveHorizontally
                        }


overlaps : Array Int -> PositionedRock -> Bool
overlaps occupied { x, y, rock } =
    let
        occupiedSlice =
            Array.slice (y - rock.height + 2) (y + 2) occupied
    in
    List.map2 (shift rock.width x >> Bitwise.and)
        rock.bits
        (Array.toList occupiedSlice)
        |> List.any ((/=) 0)


occupy : Array Int -> Dict Int Int -> PositionedRock -> ( Array Int, Dict Int Int )
occupy occupied counts { x, y, rock } =
    let
        start =
            y - rock.height + 2

        newOccupied =
            rock.bits
                |> List.indexedMap (\index bits -> ( start + index, bits ))
                |> List.foldl
                    (\( nextY, bits ) nextOccupied ->
                        let
                            newBits =
                                Array.get nextY nextOccupied
                                    |> Maybe.withDefault 0
                                    |> Bitwise.or (bits |> shift rock.width x)
                        in
                        if nextY >= Array.length nextOccupied then
                            Array.push newBits nextOccupied

                        else
                            Array.set nextY newBits nextOccupied
                    )
                    occupied

        newCounts =
            List.range start (Array.length newOccupied - 1)
                |> List.foldl
                    (\i acc ->
                        acc
                            |> Dict.update i
                                (\maybeCount ->
                                    case maybeCount of
                                        Just count ->
                                            Just (count + 1)

                                        Nothing ->
                                            Dict.get (i - 1) acc
                                                |> (if i == start then
                                                        Maybe.map ((+) 1)

                                                    else
                                                        identity
                                                   )
                                )
                    )
                    counts
    in
    ( newOccupied, newCounts )


shift : Int -> Int -> Int -> Int
shift width x =
    Bitwise.shiftLeftBy (chamberWidth - width - x)


type alias Repeat =
    { rocksBefore : Int
    , rocksInLoop : Int
    , y : Int
    , height : Int
    }


repeatRegex : Regex
repeatRegex =
    Regex.fromString "((?:,\\d+){10,})\\1\\b"
        |> Maybe.withDefault Regex.never


findRepeat : Array Int -> Dict Int Int -> Maybe Repeat
findRepeat occupied counts =
    let
        string =
            occupied
                |> Array.toList
                |> List.map String.fromInt
                |> String.join ","
    in
    case Regex.findAtMost 1 repeatRegex string of
        [ match ] ->
            let
                itemsBefore =
                    String.left match.index string
                        |> String.split ","
                        |> List.length

                itemsInLoopRaw =
                    match.match
                        |> String.split ","
                        |> List.length

                itemsInLoop =
                    (itemsInLoopRaw - 1) // 2

                rocksBefore =
                    Dict.get (itemsBefore - 1) counts
                        |> Maybe.withDefault 0

                rocksInLoop =
                    Dict.get (itemsBefore + itemsInLoop - 1) counts
                        |> Maybe.withDefault 0
                        |> (+) -rocksBefore
            in
            Just
                { rocksBefore = rocksBefore
                , rocksInLoop = rocksInLoop

                -- Remove 1 because `occupied` contains the bottom edge as well.
                , y = itemsBefore - 1 + itemsInLoop
                , height = itemsInLoop
                }

        _ ->
            Nothing


findAndUpdateRepeat : Model -> Model
findAndUpdateRepeat model =
    case model.repeat of
        Just _ ->
            model

        Nothing ->
            -- `findRepeat` is very expensive, so don’t run it every time.
            if model.rockCounter |> modBy (Array.length model.gas) |> (==) 0 then
                case findRepeat model.occupied model.counts of
                    Just repeat ->
                        let
                            repeatsToSkip =
                                -- Cannot use `//` since it truncates to 32 bit.
                                toFloat (partToLimit model.part - model.rockCounter) / toFloat repeat.rocksInLoop |> floor

                            rocksToSkip =
                                repeatsToSkip * repeat.rocksInLoop
                        in
                        { model
                            | rockCounter = model.rockCounter + rocksToSkip
                            , repeat =
                                Just
                                    { repeat = repeat
                                    , repeatsToSkip = repeatsToSkip
                                    }
                        }

                    Nothing ->
                        model

            else
                model


getStoppedY : List PositionedRock -> Int
getStoppedY =
    List.map (\{ y } -> y)
        >> List.maximum
        >> Maybe.withDefault 0


stepToEnd : Model -> Model
stepToEnd model =
    if model.playing then
        stepToEnd (step model)

    else
        model


fpsMin : Int
fpsMin =
    1


limitMin : Int
limitMin =
    1


view : Model -> Html Msg
view model =
    let
        stoppedY =
            getStoppedY model.stopped
    in
    Html.div []
        [ viewSvg stoppedY model
        , viewControls stoppedY model
        ]


viewControls : Int -> Model -> Html Msg
viewControls stoppedY model =
    let
        repeatHeight =
            case model.repeat of
                Just { repeat, repeatsToSkip } ->
                    repeat.height * repeatsToSkip

                Nothing ->
                    0

        height =
            stoppedY
                + (if stoppedY == 0 then
                    0

                   else
                    1
                  )
                + repeatHeight

        done =
            model.rockCounter >= partToLimit model.part
    in
    Html.div [ Html.Attributes.id "controls-wrapper" ]
        [ Html.div [ Html.Attributes.id "controls" ]
            [ Html.button [ Html.Events.onClick PlayPauseToggled, Html.Attributes.disabled done ]
                [ Html.text
                    (if model.playing then
                        "⏸"

                     else
                        "▶️"
                    )
                ]
            , Html.button [ Html.Events.onClick NextStepClicked, Html.Attributes.disabled done ]
                [ Html.text "⏩" ]
            , if model.steppingToEnd then
                Html.button []
                    [ Html.text "⏳" ]

              else
                Html.button [ Html.Events.onClick StepToEndClicked, Html.Attributes.disabled done ]
                    [ Html.text "⏭" ]
            , Html.button [ Html.Events.onClick (SwitchPartClicked model.part) ]
                [ Html.text "↩️" ]
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
        , Html.div [] [ Html.text ("Height: " ++ String.fromInt height) ]
        , Html.div [] [ Html.text ("Next rock: " ++ String.fromInt model.rockCounter) ]
        , Html.div []
            [ Html.text
                ("Next gas: "
                    ++ (case Array.get model.gasIndex model.gas of
                            Nothing ->
                                "(none)"

                            Just Left ->
                                "<"

                            Just Right ->
                                ">"
                       )
                )
            ]
        , Html.div []
            [ Html.text
                ("Next move: "
                    ++ (case model.movement of
                            MoveHorizontally ->
                                "↔️"

                            MoveDown ->
                                "⬇️"
                       )
                )
            ]
        , Html.div [ Html.Attributes.class "vertical" ]
            (([ ( ExampleA, "Example Part 1" )
              , ( ExampleB, "Example Part 2" )
              , ( FullA, "Full Part 1" )
              , ( FullB, "Full Part 2" )
              ]
                |> List.map
                    (\( part, label ) ->
                        Html.label []
                            [ Html.input
                                [ Html.Attributes.type_ "radio"
                                , Html.Attributes.name "part"
                                , Html.Events.onCheck (always (SwitchPartClicked part))
                                , Html.Attributes.checked (model.part == part)
                                ]
                                []
                            , Html.text label
                            ]
                    )
             )
                ++ [ Html.label []
                        [ Html.input
                            [ Html.Attributes.type_ "radio"
                            , Html.Attributes.name "part"
                            , Html.Events.onCheck (always SwitchToCustomClicked)
                            , Html.Attributes.checked
                                (case model.part of
                                    Custom _ ->
                                        True

                                    _ ->
                                        False
                                )
                            ]
                            []
                        , Html.text "Custom"
                        ]
                   ]
            )
        , case model.part of
            Custom _ ->
                Html.div [ Html.Attributes.class "vertical" ]
                    [ Html.label []
                        [ Html.text "Limit: "
                        , Html.input
                            [ Html.Attributes.type_ "number"
                            , Html.Attributes.min (String.fromInt limitMin)
                            , Html.Attributes.value model.limitInput
                            , Html.Events.onInput LimitInputChanged
                            , Html.Attributes.style "width" "4em"
                            ]
                            []
                        ]
                    , Html.textarea
                        [ Html.Attributes.placeholder "Paste input here"
                        , Html.Attributes.style "width" "11em"
                        , Html.Events.onInput GasInputChanged
                        , Html.Attributes.value model.gasInput
                        ]
                        []
                    , case model.gasInputError of
                        Just error ->
                            Html.pre [] [ Html.text error ]

                        Nothing ->
                            Html.text ""
                    ]

            _ ->
                Html.text ""
        ]


chamberWidth : Int
chamberWidth =
    7


startLeft : Int
startLeft =
    2


startBottom : Int
startBottom =
    3


viewSvg : Int -> Model -> Html Msg
viewSvg stoppedY model =
    let
        ( currentSvg, currentY ) =
            case model.current of
                Just { x, y, rock } ->
                    ( Svg.g
                        [ Svg.Attributes.transform ("translate( " ++ String.fromInt x ++ " " ++ String.fromInt -y ++ ")")
                        , Svg.Attributes.style ("transition: transform " ++ String.fromFloat (1000 / toFloat model.fps) ++ "ms ease")
                        , Svg.Attributes.id currentSvgId
                        ]
                        [ viewRock 0 0 [] rock ]
                    , y
                    )

                Nothing ->
                    ( Svg.text "", 0 )

        offsetY =
            max stoppedY currentY

        { chamberHeight } =
            model

        padding =
            2

        viewBox =
            [ 0 - padding
            , -chamberHeight - padding
            , chamberWidth + 1 + 2 * padding
            , chamberHeight + 2 * padding
            ]
                |> List.map String.fromInt
                |> String.join " "
                |> Svg.Attributes.viewBox
    in
    Svg.Keyed.node "svg"
        [ viewBox ]
        [ ( "viewChamber", viewChamber chamberHeight )
        , ( "viewStopped", viewStopped model.stopped )
        , ( "viewRepeat"
          , case model.repeat of
                Just repeatData ->
                    viewRepeat repeatData

                Nothing ->
                    Svg.text ""
          )
        , ( "viewRemaining" ++ String.fromInt model.rockCounter, viewRemaining model.fps offsetY (model.rockCounter + 1) (partToLimit model.part) |> Tuple.first )
        , ( "currentSvg" ++ String.fromInt model.rockCounter, currentSvg )
        ]


viewChamber : Int -> Svg msg
viewChamber chamberHeight =
    Svg.rect
        [ Svg.Attributes.x "-0.5"
        , Svg.Attributes.y (String.fromFloat -(toFloat chamberHeight + 0.5))
        , Svg.Attributes.width (String.fromInt (chamberWidth + 1))
        , Svg.Attributes.height (String.fromInt (chamberHeight + 2))
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeWidth "1"
        ]
        []


viewRepeat : RepeatData -> Svg msg
viewRepeat { repeat, repeatsToSkip } =
    Svg.g []
        [ Svg.rect
            [ Svg.Attributes.x "0"

            -- Remove `1` since `y=0` confusingly is one square _up_ from the bottom white edge.
            , Svg.Attributes.y (String.fromInt -(repeat.y - 1))
            , Svg.Attributes.width (String.fromInt chamberWidth)
            , Svg.Attributes.height (String.fromInt repeat.height)
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "magenta"
            , Svg.Attributes.strokeWidth "0.25"
            ]
            []
        , Svg.text_
            [ Svg.Attributes.x (String.fromFloat (toFloat chamberWidth + 1.5))
            , Svg.Attributes.y (String.fromInt -(repeat.y - 2))
            , Svg.Attributes.fill "magenta"
            , Svg.Attributes.fontSize "1"
            ]
            [ Svg.text ("* " ++ String.fromInt repeatsToSkip) ]
        ]


viewRemaining : Int -> Int -> Int -> Int -> ( Svg msg, Int )
viewRemaining fps offset start end =
    List.range start (min 3000 end)
        |> List.foldl
            (\i ( acc, bottom ) ->
                let
                    rock =
                        oneBasedIntToRock i

                    y =
                        bottom + rock.height + startBottom
                in
                ( ( String.fromInt i, viewRock startLeft -y [] rock ) :: acc
                , y
                )
            )
            ( [], 0 )
        |> Tuple.mapFirst
            (Svg.Keyed.node "g"
                [ Svg.Attributes.transform ("translate(0 " ++ String.fromInt -offset ++ ")")
                , Svg.Attributes.style ("transition: transform " ++ String.fromFloat (1000 / toFloat fps) ++ "ms ease")
                ]
            )


viewStopped : List PositionedRock -> Svg msg
viewStopped stoppedRocks =
    Svg.Keyed.node "g"
        []
        (stoppedRocks
            |> List.map
                (\{ x, y, rock } ->
                    ( String.fromInt x ++ "," ++ String.fromInt y
                    , viewRock x -y [ Svg.Attributes.class "stopped" ] rock
                    )
                )
        )


viewRock : Int -> Int -> List (Svg.Attribute msg) -> Rock -> Svg msg
viewRock x y attributes rock =
    Svg.path
        ([ Svg.Attributes.d (commandsToDString (MoveAbsolute x y :: variantToPathCommands rock.variant))
         , Svg.Attributes.fill (variantToColor rock.variant)
         ]
            ++ attributes
        )
        []


type Command
    = Move Int Int
    | MoveAbsolute Int Int
    | Horizontal Int
    | Vertical Int


variantToPathCommands : Variant -> List Command
variantToPathCommands variant =
    case variant of
        Minus ->
            [ Horizontal 4, Vertical 1, Horizontal -4, Vertical -1 ]

        Plus ->
            [ Move 1 0, Horizontal 1, Vertical 1, Horizontal 1, Vertical 1, Horizontal -1, Vertical 1, Horizontal -1, Vertical -1, Horizontal -1, Vertical -1, Horizontal 1, Vertical -1 ]

        J ->
            [ Move 2 0, Horizontal 1, Vertical 3, Horizontal -3, Vertical -1, Horizontal 2, Vertical -2 ]

        I ->
            [ Horizontal 1, Vertical 4, Horizontal -1, Vertical -4 ]

        Square ->
            [ Horizontal 2, Vertical 2, Horizontal -2, Vertical -2 ]


commandsToDString : List Command -> String
commandsToDString commands =
    commands
        |> List.map
            (\command ->
                case command of
                    Move x y ->
                        "m " ++ String.fromInt x ++ " " ++ String.fromInt y

                    MoveAbsolute x y ->
                        "M " ++ String.fromInt x ++ " " ++ String.fromInt y

                    Horizontal x ->
                        "h " ++ String.fromInt x

                    Vertical y ->
                        "v " ++ String.fromInt y
            )
        |> String.join " "

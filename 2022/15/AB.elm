module AB exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Input
import Process
import Set
import Svg exposing (Svg)
import Svg.Attributes
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { readings : List Reading
    , rules : Rules
    , input : String
    , inputError : Maybe String
    , solutionB : Maybe (Result Error ( Int, Int ))
    }


type alias Reading =
    { sensor : ( Int, Int )
    , beacon : ( Int, Int )
    }


type Rules
    = Example
    | Full


type alias RulesConfig =
    { scanLine : Int
    , upperLimit : Int
    }


getRulesConfig : Rules -> RulesConfig
getRulesConfig rules =
    case rules of
        Example ->
            { scanLine = 10, upperLimit = 20 }

        Full ->
            { scanLine = 2000000, upperLimit = 4000000 }


type Msg
    = InputChanged String
    | SwitchRulesClicked Rules
    | SolveB Int


init : () -> ( Model, Cmd Msg )
init () =
    initRules
        { readings = []
        , rules = Example
        , input = Input.example
        , inputError = Nothing
        , solutionB = Nothing
        }


initRules : Model -> ( Model, Cmd Msg )
initRules model =
    case parse model.input of
        Ok readings ->
            ( { model
                | readings = readings
                , inputError = Nothing
                , solutionB = Nothing
              }
            , scheduleB 0
            )

        Err errors ->
            ( { model | inputError = Just (String.join "\n" errors) }, Cmd.none )


scheduleB : Int -> Cmd Msg
scheduleB currentY =
    Process.sleep 10 |> Task.attempt (always (SolveB currentY))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged input ->
            initRules
                { model
                    | input =
                        if String.isEmpty (String.trim input) then
                            Input.input

                        else
                            input
                }

        SwitchRulesClicked rules ->
            initRules
                { model
                    | rules = rules
                    , input =
                        case rules of
                            Example ->
                                Input.example

                            Full ->
                                Input.input
                }

        SolveB currentY ->
            let
                config =
                    getRulesConfig model.rules

                limit =
                    min config.upperLimit (currentY + 400000)
            in
            case solveB limit currentY model.readings of
                Ok coordinate ->
                    ( { model | solutionB = Just (Ok coordinate) }
                    , Cmd.none
                    )

                Err LimitReached ->
                    if limit == config.upperLimit then
                        ( { model | solutionB = Just (Err LimitReached) }
                        , Cmd.none
                        )

                    else
                        ( model, scheduleB (limit + 1) )

                Err (UnexpectedError error) ->
                    ( { model | solutionB = Just (Err (UnexpectedError error)) }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


parse : String -> Result (List String) (List Reading)
parse input =
    input
        |> String.trim
        |> String.replace "," ""
        |> String.replace ":" ""
        |> String.replace "=" " "
        |> String.lines
        |> List.indexedMap
            (\lineIndex line ->
                case String.split " " line of
                    [ "Sensor", "at", "x", sensorXString, "y", sensorYString, "closest", "beacon", "is", "at", "x", beaconXString, "y", beaconYString ] ->
                        case ( ( String.toInt sensorXString, String.toInt sensorYString ), ( String.toInt beaconXString, String.toInt beaconYString ) ) of
                            ( ( Just sensorX, Just sensorY ), ( Just beaconX, Just beaconY ) ) ->
                                Ok
                                    { sensor = ( sensorX, sensorY )
                                    , beacon = ( beaconX, beaconY )
                                    }

                            _ ->
                                Err ("Line " ++ String.fromInt (lineIndex + 1) ++ ": Invalid ints: " ++ line)

                    _ ->
                        Err ("Line " ++ String.fromInt (lineIndex + 1) ++ ": Did not match the expected pattern: " ++ line)
            )
        |> collectResults


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


view : Model -> Html Msg
view model =
    Html.div []
        [ viewSvg model
        , viewControls model
        ]


viewControls : Model -> Html Msg
viewControls model =
    let
        config =
            getRulesConfig model.rules
    in
    Html.div [ Html.Attributes.id "controls-wrapper" ]
        [ Html.div [ Html.Attributes.id "controls" ]
            [ Html.label []
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.name "part"
                    , Html.Events.onCheck (always (SwitchRulesClicked Example))
                    , Html.Attributes.checked (model.rules == Example)
                    ]
                    []
                , Html.text "Example"
                ]
            , Html.label []
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.name "part"
                    , Html.Events.onCheck (always (SwitchRulesClicked Full))
                    , Html.Attributes.checked (model.rules == Full)
                    ]
                    []
                , Html.text "Full"
                ]
            ]
        , Html.div [] [ Html.text ("Part 1: " ++ String.fromInt (solveA config.scanLine model.readings)) ]
        , Html.div [] [ Html.text ("Part 2: " ++ solutionBToString model.solutionB) ]
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


solutionBToString : Maybe (Result Error ( Int, Int )) -> String
solutionBToString maybe =
    case maybe of
        Just (Ok ( x, y )) ->
            String.fromInt (x * 4000000 + y)

        Just (Err LimitReached) ->
            "Limit reached"

        Just (Err (UnexpectedError error)) ->
            error

        Nothing ->
            "Calculating…"


type alias Bounds =
    { xMin : Int
    , xMax : Int
    , yMin : Int
    , yMax : Int
    }


readingsToBounds : Int -> List Reading -> Bounds
readingsToBounds upperLimit readings =
    let
        ( xs, ys ) =
            readings
                |> List.concatMap (\reading -> [ reading.sensor, reading.beacon ])
                |> List.append [ ( 0, 0 ), ( upperLimit, upperLimit ) ]
                |> List.unzip
    in
    { xMin = minimum xs
    , xMax = maximum xs
    , yMin = minimum ys
    , yMax = maximum ys
    }


minimum : List Int -> Int
minimum =
    List.minimum >> Maybe.withDefault 0


maximum : List Int -> Int
maximum =
    List.maximum >> Maybe.withDefault 0


viewSvg : Model -> Svg msg
viewSvg model =
    let
        config =
            getRulesConfig model.rules

        { readings } =
            model

        bounds =
            readingsToBounds config.upperLimit readings

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
    Svg.svg [ viewBox ]
        ((if max (bounds.xMax - bounds.xMin) (bounds.yMax - bounds.yMin) < 100 then
            viewGrid bounds

          else
            []
         )
            ++ List.map viewSensorCoverage readings
            ++ [ viewScanLine bounds config.scanLine
               , viewSearchSpace config.upperLimit
               , case model.solutionB of
                    Just (Ok spot) ->
                        viewItem "magenta" "square" [ Svg.Attributes.id "distress" ] spot

                    _ ->
                        Svg.text ""
               ]
            ++ (readings
                    |> List.concatMap
                        (\reading ->
                            [ viewConnectionLine reading.sensor reading.beacon
                            , viewItem "steelblue" "round" [] reading.sensor
                            , viewItem "gold" "square" [] reading.beacon
                            ]
                        )
               )
        )


viewSearchSpace : Int -> Svg msg
viewSearchSpace upperLimit =
    Svg.rect
        [ Svg.Attributes.x "-0.5"
        , Svg.Attributes.y "-0.5"
        , Svg.Attributes.width (String.fromInt (upperLimit + 1))
        , Svg.Attributes.height (String.fromInt (upperLimit + 1))
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeWidth "1"
        , Html.Attributes.attribute "vector-effect" "non-scaling-stroke"
        ]
        []


viewGrid : Bounds -> List (Svg msg)
viewGrid bounds =
    let
        verticalLines =
            List.range bounds.xMin (bounds.xMax + 1)
                |> List.map
                    (\x ->
                        ( ( toFloat x - 0.5, toFloat bounds.yMin - 0.5 )
                        , ( toFloat x - 0.5, toFloat bounds.yMax + 0.5 )
                        )
                    )

        horizontalLines =
            List.range bounds.yMin (bounds.yMax + 1)
                |> List.map
                    (\y ->
                        ( ( toFloat bounds.xMin - 0.5, toFloat y - 0.5 )
                        , ( toFloat bounds.xMax + 0.5, toFloat y - 0.5 )
                        )
                    )
    in
    verticalLines
        ++ horizontalLines
        |> List.map
            (\( ( x1, y1 ), ( x2, y2 ) ) ->
                Svg.line
                    [ Svg.Attributes.x1 (String.fromFloat x1)
                    , Svg.Attributes.y1 (String.fromFloat y1)
                    , Svg.Attributes.x2 (String.fromFloat x2)
                    , Svg.Attributes.y2 (String.fromFloat y2)
                    , Svg.Attributes.stroke "rgba(255, 255, 255, 0.2)"
                    , Svg.Attributes.strokeWidth "1"
                    , Html.Attributes.attribute "vector-effect" "non-scaling-stroke"
                    ]
                    []
            )


viewScanLine : Bounds -> Int -> Svg msg
viewScanLine bounds scanLine =
    let
        x =
            toFloat bounds.xMin - 0.5

        y =
            toFloat scanLine - 0.5

        width =
            bounds.xMax - bounds.xMin + 1
    in
    Svg.rect
        [ Svg.Attributes.x (String.fromFloat x)
        , Svg.Attributes.y (String.fromFloat y)
        , Svg.Attributes.width (String.fromInt width)
        , Svg.Attributes.height "1"
        , Svg.Attributes.fill "rgba(255, 0, 0, 0.3)"
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeWidth "1"
        , Html.Attributes.attribute "vector-effect" "non-scaling-stroke"
        ]
        []


viewConnectionLine : ( Int, Int ) -> ( Int, Int ) -> Svg msg
viewConnectionLine ( x1, y1 ) ( x2, y2 ) =
    Svg.line
        [ Svg.Attributes.x1 (String.fromInt x1)
        , Svg.Attributes.y1 (String.fromInt y1)
        , Svg.Attributes.x2 (String.fromInt x2)
        , Svg.Attributes.y2 (String.fromInt y2)
        , Svg.Attributes.stroke "rgba(255, 255, 255, 0.5)"
        , Svg.Attributes.strokeWidth "1"
        , Html.Attributes.attribute "vector-effect" "non-scaling-stroke"
        ]
        []


viewItem : String -> String -> List (Svg.Attribute msg) -> ( Int, Int ) -> Svg msg
viewItem color shape attributes ( x, y ) =
    -- https://muffinman.io/blog/svg-non-scaling-circle-and-rectangle/
    let
        d =
            "M"
                ++ String.fromInt x
                ++ ","
                ++ String.fromInt y
                ++ "l0.0001,0"
                |> Svg.Attributes.d
    in
    Svg.path
        ([ d
         , Svg.Attributes.stroke color
         , Svg.Attributes.strokeLinecap shape
         , Svg.Attributes.strokeWidth "10"
         , Html.Attributes.attribute "vector-effect" "non-scaling-stroke"
         ]
            ++ attributes
        )
        []


viewSensorCoverage : Reading -> Svg msg
viewSensorCoverage reading =
    let
        distance =
            manhattanDistance reading.sensor reading.beacon

        ( x, y ) =
            reading.sensor

        points =
            [ ( x, y - distance )
            , ( x + distance, y )
            , ( x, y + distance )
            , ( x - distance, y )
            ]
                |> List.map
                    (\( a, b ) ->
                        String.fromInt a ++ "," ++ String.fromInt b
                    )
                |> String.join " "
                |> Svg.Attributes.points
    in
    Svg.polygon
        [ points
        , Svg.Attributes.fill "steelblue"
        , Svg.Attributes.opacity "0.45"
        ]
        []


manhattanDistance : ( Int, Int ) -> ( Int, Int ) -> Int
manhattanDistance ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)


solveA : Int -> List Reading -> Int
solveA scanLine readings =
    let
        scanCoverage =
            getNonOverlappingRanges scanLine readings
                |> List.map (\( start, end ) -> end - start + 1)
                |> List.sum

        numBeaconsInScanLine =
            readings
                |> List.map (.beacon >> Tuple.second)
                |> List.filter ((==) scanLine)
                |> Set.fromList
                |> Set.size
    in
    scanCoverage - numBeaconsInScanLine


getNonOverlappingRanges : Int -> List Reading -> List ( Int, Int )
getNonOverlappingRanges scanLine readings =
    readings
        |> List.filterMap
            (\reading ->
                let
                    distanceToBeacon =
                        manhattanDistance reading.sensor reading.beacon

                    ( x, y ) =
                        reading.sensor

                    distanceToScanLine =
                        abs (scanLine - y)
                in
                if distanceToScanLine <= distanceToBeacon then
                    let
                        rest =
                            distanceToBeacon - distanceToScanLine
                    in
                    Just ( x - rest, x + rest )

                else
                    Nothing
            )
        |> List.sort
        |> mergeSortedRanges


mergeSortedRanges : List ( Int, Int ) -> List ( Int, Int )
mergeSortedRanges ranges =
    case ranges of
        ( start1, end1 ) :: ( start2, end2 ) :: rest ->
            if start2 <= end1 then
                mergeSortedRanges (( start1, max end1 end2 ) :: rest)

            else
                ( start1, end1 ) :: mergeSortedRanges (( start2, end2 ) :: rest)

        _ ->
            ranges


type Error
    = LimitReached
    | UnexpectedError String


solveB : Int -> Int -> List Reading -> Result Error ( Int, Int )
solveB upperLimit y readings =
    if y > upperLimit then
        Err LimitReached

    else
        case getNonOverlappingRanges y readings of
            [ _ ] ->
                solveB upperLimit (y + 1) readings

            [ ( _, end ), ( start, _ ) ] as ranges ->
                if (start - end) == 2 then
                    Ok ( end + 1, y )

                else
                    Err (UnexpectedError ("Expected a gap of 2: " ++ rangesToString ranges))

            ranges ->
                Err (UnexpectedError ("Unexpected number of non-overlapping ranges: " ++ rangesToString ranges))


rangesToString : List ( Int, Int ) -> String
rangesToString =
    List.map (\( a, b ) -> String.fromInt a ++ "," ++ String.fromInt b)
        >> String.join " ; "

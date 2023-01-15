module Day12 exposing (..)

import Html exposing (Html)
import Html.Attributes
import LineParser
import Set
import String
import Svg exposing (Svg)
import Svg.Attributes


type Action
    = GoNorth Int
    | GoSouth Int
    | GoEast Int
    | GoWest Int
    | TurnLeft Turns
    | TurnRight Turns
    | GoForward Int


type Turns
    = One
    | Two
    | Three


actionToString : Action -> String
actionToString action =
    case action of
        GoNorth int ->
            "N" ++ String.fromInt int

        GoSouth int ->
            "S" ++ String.fromInt int

        GoEast int ->
            "E" ++ String.fromInt int

        GoWest int ->
            "W" ++ String.fromInt int

        TurnLeft turns ->
            "L" ++ turnsToString turns

        TurnRight turns ->
            "R" ++ turnsToString turns

        GoForward int ->
            "F" ++ String.fromInt int


turnsToString : Turns -> String
turnsToString turns =
    case turns of
        One ->
            "90"

        Two ->
            "180"

        Three ->
            "270"


leftToRight : Turns -> Turns
leftToRight turns =
    case turns of
        One ->
            Three

        Two ->
            Two

        Three ->
            One


parse : String -> Result String (List Action)
parse =
    LineParser.parse
        (\line ->
            case String.uncons line of
                Just ( 'N', arg ) ->
                    withInt GoNorth arg

                Just ( 'S', arg ) ->
                    withInt GoSouth arg

                Just ( 'E', arg ) ->
                    withInt GoEast arg

                Just ( 'W', arg ) ->
                    withInt GoWest arg

                Just ( 'L', arg ) ->
                    withTurns TurnLeft arg

                Just ( 'R', arg ) ->
                    withTurns TurnRight arg

                Just ( 'F', arg ) ->
                    withInt GoForward arg

                Just ( other, _ ) ->
                    Err ("Unknown action: " ++ String.fromChar other)

                Nothing ->
                    Err "Unexpected empty string."
        )


withInt : (Int -> Action) -> String -> Result String Action
withInt constructor arg =
    case String.toInt arg of
        Just int ->
            Ok (constructor int)

        Nothing ->
            Err ("Expected an int but got: " ++ arg)


withTurns : (Turns -> Action) -> String -> Result String Action
withTurns constructor arg =
    case String.toInt arg of
        Just 90 ->
            Ok (constructor One)

        Just 180 ->
            Ok (constructor Two)

        Just 270 ->
            Ok (constructor Three)

        _ ->
            Err ("Expected an 90, 180 or 270 but got: " ++ arg)


type alias State =
    { east : Int
    , north : Int
    , direction : Direction
    }


type Direction
    = North
    | South
    | East
    | West


emptyState : State
emptyState =
    { east = 0
    , north = 0
    , direction = East
    }


step : Action -> State -> State
step action state =
    case action of
        GoNorth int ->
            { state | north = state.north + int }

        GoSouth int ->
            { state | north = state.north - int }

        GoEast int ->
            { state | east = state.east + int }

        GoWest int ->
            { state | east = state.east - int }

        TurnLeft turns ->
            { state | direction = turnRight (leftToRight turns) state.direction }

        TurnRight turns ->
            { state | direction = turnRight turns state.direction }

        GoForward int ->
            case state.direction of
                North ->
                    { state | north = state.north + int }

                South ->
                    { state | north = state.north - int }

                East ->
                    { state | east = state.east + int }

                West ->
                    { state | east = state.east - int }


turnRight : Turns -> Direction -> Direction
turnRight turns direction =
    case turns of
        One ->
            case direction of
                North ->
                    East

                South ->
                    West

                East ->
                    South

                West ->
                    North

        Two ->
            direction
                |> turnRight One
                |> turnRight One

        Three ->
            direction
                |> turnRight One
                |> turnRight One
                |> turnRight One


type alias State2 =
    { east : Int
    , north : Int
    , waypointEast : Int
    , waypointNorth : Int
    }


emptyState2 : State2
emptyState2 =
    { east = 0
    , north = 0
    , waypointEast = 10
    , waypointNorth = 1
    }


step2 : Action -> State2 -> State2
step2 action state =
    case action of
        GoNorth int ->
            { state | waypointNorth = state.waypointNorth + int }

        GoSouth int ->
            { state | waypointNorth = state.waypointNorth - int }

        GoEast int ->
            { state | waypointEast = state.waypointEast + int }

        GoWest int ->
            { state | waypointEast = state.waypointEast - int }

        TurnLeft turns ->
            turnRight2 (leftToRight turns) state

        TurnRight turns ->
            turnRight2 turns state

        GoForward int ->
            { state
                | north = state.north + state.waypointNorth * int
                , east = state.east + state.waypointEast * int
            }


turnRight2 : Turns -> State2 -> State2
turnRight2 turns state =
    case turns of
        One ->
            { state
                | waypointEast = state.waypointNorth
                , waypointNorth = -state.waypointEast
            }

        Two ->
            state
                |> turnRight2 One
                |> turnRight2 One

        Three ->
            state
                |> turnRight2 One
                |> turnRight2 One
                |> turnRight2 One


manhattanDistance : { a | east : Int, north : Int } -> Int
manhattanDistance state =
    abs state.east + abs state.north


main : Html Never
main =
    case parse puzzleInput of
        Ok actions ->
            viewActions actions

        Err error ->
            Html.text error


viewActions : List Action -> Html msg
viewActions actions =
    let
        ( finalState, allStates ) =
            List.foldl
                (\action ( state, states ) ->
                    let
                        nextState =
                            step action state
                    in
                    ( nextState, nextState :: states )
                )
                ( emptyState, [] )
                actions
                |> Tuple.mapSecond List.reverse

        segments : List Segment
        segments =
            List.map3
                (\state previousState action ->
                    { x = state.east
                    , y = -state.north
                    , prevX = previousState.east
                    , prevY = -previousState.north
                    , action = action
                    }
                )
                allStates
                (emptyState :: allStates)
                actions

        finalState2 =
            List.foldl step2 emptyState2 actions
    in
    Html.div []
        [ Html.div []
            [ Html.text
                (String.fromInt (manhattanDistance finalState)
                    ++ " / "
                    ++ String.fromInt (manhattanDistance finalState2)
                )
            ]
        , Html.div []
            [ viewSvg segments ]
        ]


type alias Segment =
    { x : Int
    , y : Int
    , prevX : Int
    , prevY : Int
    , action : Action
    }


viewSvg : List Segment -> Svg msg
viewSvg segments =
    let
        xs =
            0 :: List.map .x segments

        ys =
            0 :: List.map .y segments

        minX =
            xs |> List.minimum |> Maybe.withDefault 0

        maxX =
            xs |> List.maximum |> Maybe.withDefault 0

        minY =
            ys |> List.minimum |> Maybe.withDefault 0

        maxY =
            ys |> List.maximum |> Maybe.withDefault 0

        width =
            maxX - minX

        height =
            maxY - minY

        viewBox =
            [ minX, minY, width, height ]
                |> List.map String.fromInt
                |> String.join " "

        pxWidth =
            1800

        fontSize =
            toFloat width / pxWidth * 20

        d =
            segments
                |> List.map
                    (\segment ->
                        "L " ++ String.fromInt segment.x ++ "," ++ String.fromInt segment.y
                    )
                |> String.join " "
    in
    Svg.svg
        [ Svg.Attributes.viewBox viewBox
        , Html.Attributes.style "width" (String.fromFloat pxWidth ++ "px")
        ]
        (Svg.path
            [ Svg.Attributes.d ("M 0,0 " ++ d)
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "white"
            , Svg.Attributes.strokeWidth "2"
            , Html.Attributes.attribute "vector-effect" "non-scaling-stroke"
            , Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.strokeLinejoin "round"
            ]
            []
            :: Svg.text_
                [ Svg.Attributes.x "0"
                , Svg.Attributes.y "0"
                , Svg.Attributes.fill "white"
                , Svg.Attributes.fontSize (String.fromFloat fontSize)
                , Svg.Attributes.dy (String.fromFloat (fontSize / 4))
                , Svg.Attributes.textAnchor "middle"
                , Html.Attributes.style "text-shadow" "1px 1px 2px black, -1px -1px 2px black, -1px 1px 2px black, 1px -1px 2px black"
                ]
                [ Svg.text "START" ]
            :: List.map (viewAction fontSize) segments
        )


viewAction : Float -> Segment -> Svg msg
viewAction fontSize segment =
    let
        def =
            ( mid segment.prevX segment.x
            , mid segment.prevY segment.y
            )

        ( x, y ) =
            case segment.action of
                GoNorth _ ->
                    def

                GoSouth _ ->
                    def

                GoEast _ ->
                    def

                GoWest _ ->
                    def

                TurnLeft _ ->
                    ( segment.x, segment.y )

                TurnRight _ ->
                    ( segment.x, segment.y )

                GoForward _ ->
                    def
    in
    Svg.text_
        [ Svg.Attributes.x (String.fromInt x)
        , Svg.Attributes.y (String.fromInt y)
        , Svg.Attributes.fill "white"
        , Svg.Attributes.fontSize (String.fromFloat fontSize)
        , Svg.Attributes.dy (String.fromFloat (fontSize / 4))
        , Svg.Attributes.textAnchor "middle"
        , Html.Attributes.style "text-shadow" "1px 1px 2px black, -1px -1px 2px black, -1px 1px 2px black, 1px -1px 2px black"
        ]
        [ Svg.text (actionToString segment.action) ]


mid : Int -> Int -> Int
mid a b =
    (a + b) // 2


shortInput : String
shortInput =
    """
F10
N3
F7
R90
F11
"""


puzzleInput : String
puzzleInput =
    """
L90
F67
R270
W1
R180
F5
E5
F59
E4
L180
F70
S2
F35
N3
E5
F58
L90
N1
F46
R90
S1
R90
E1
L180
W4
F99
N2
F84
N1
R90
N5
W4
F26
E1
F97
N1
F36
W1
F21
S4
F31
S3
F76
S5
S1
L90
S4
W4
R90
E4
F14
R90
S2
R90
S3
F21
N1
W4
S4
E1
L180
N5
F30
N3
F4
N5
F100
N2
R270
E1
S1
F79
N4
F72
W4
F50
L90
W5
S4
E2
N5
E4
S5
W5
L90
E4
L90
S4
E4
R90
N1
W5
R270
W5
N4
R180
E5
F86
L90
W3
F79
W5
F87
L180
N4
E2
S1
W3
N3
F31
W2
N1
F86
E1
L90
L90
F2
E3
F8
L90
F54
W3
S5
E3
F89
N5
R90
E3
F70
N2
R90
F55
W3
R90
F44
E2
F36
L90
E3
S2
F23
N4
F2
W5
L180
E4
N4
W3
F58
W1
R90
W1
L90
E1
F99
W4
S4
E5
N2
R180
E5
F82
N3
F99
L90
N4
E4
S5
R90
N3
F17
S5
E4
F58
E1
N1
E5
R90
F32
N1
R90
F84
E4
W4
R180
E4
R90
N1
F26
W4
R90
F96
E5
S2
F86
R90
F95
S4
F81
R90
W4
F44
N3
W3
N5
L180
L90
F71
S4
R90
E5
N4
F63
W2
F75
N3
R90
S2
E3
F75
R90
W3
F4
L90
E3
F96
L180
F53
W5
L90
F12
N2
F100
W2
R270
S1
F37
E4
S1
E1
L270
W2
S5
F10
L90
N3
F63
L90
F96
S3
W1
N4
R180
E2
F51
L90
N4
F27
W3
N5
R90
N4
L180
F4
N1
L180
F71
E5
S5
F94
L90
F98
E3
N4
E5
R90
F75
S1
F19
E2
F53
S3
L90
F29
R180
F88
R180
F3
S2
E5
F16
L90
E1
S2
E3
F28
E5
F22
L180
S2
E1
S1
F6
E2
S3
F14
R90
N4
S5
F77
L90
N3
R90
N2
L180
F99
E2
F85
S3
F81
N1
W1
F91
F31
N5
W5
R90
S1
F40
N2
E1
S3
L90
E5
R180
E2
L90
F88
R90
F45
R270
W4
F67
W4
S1
W4
F65
L90
F19
R90
F83
S1
R90
E2
R180
F78
E1
E1
L180
S1
E1
N4
W5
F98
L90
E4
L90
N2
E1
N4
E1
N5
L90
S3
F52
W5
F55
S4
R180
F56
S5
E1
R90
F97
E5
N4
L90
E1
N1
W1
N4
L270
F7
N3
L90
W3
L270
F27
E2
N5
F90
N3
R90
F79
N4
F58
L90
W5
F90
F9
E5
R90
F16
E4
F50
S1
R90
N5
E2
F86
E3
L270
W3
L90
W1
F17
N2
L180
N1
W4
R180
F10
N3
W3
L90
E2
F12
S5
L90
N3
W4
N3
F19
E5
F54
E1
F34
F2
S4
F14
R90
S4
F2
N1
E3
N2
L180
E5
F67
L180
F66
E3
S4
W3
F51
L270
N5
F51
W3
S2
E2
N2
F27
W5
F77
E4
N5
E2
F20
N5
E4
S5
F67
S2
F81
L90
F68
E4
F71
L90
F48
N3
F1
N5
R90
F76
W5
S5
F74
S1
E2
F52
R90
W1
S4
F13
F69
L180
F59
N3
F34
F84
R90
F63
W2
L90
F12
L90
W5
F25
F83
E4
N1
R90
F36
S1
W2
F41
R90
N3
W1
R180
W2
L90
N4
F87
E3
S4
F10
S3
F33
R90
E1
L180
F32
W5
S3
F23
R90
F44
L90
F45
E2
L270
F41
W1
F54
L180
F31
R90
F43
S3
F91
F88
L180
F2
W2
N5
W2
S1
L180
F12
N2
F2
N3
W2
R90
S2
E4
F66
S2
W4
F94
S5
E1
L180
N5
F2
N2
R180
E3
F3
E1
R90
S3
F28
L90
F12
L90
S2
F100
L90
F84
E2
R90
W4
F14
N1
W3
F33
W1
N5
R180
F93
W5
N2
E4
L180
W3
F2
S1
W4
L90
F8
W2
F83
E5
R180
W4
S4
R90
E4
R180
F84
E2
N3
W3
N1
L90
F76
W1
F9
E1
S1
E5
L90
S1
S5
W4
S3
F20
N2
F52
R180
F21
W4
N2
L90
F42
S3
E5
N4
F100
E5
N5
F56
L90
F90
S1
E2
N2
F42
E3
L90
W4
R180
F22
L90
R90
F48
E4
N4
E5
F10
L90
N5
F99
S4
E3
R90
N5
E3
F85
F83
W1
R180
L90
W4
R90
W1
L90
S4
L90
N3
W5
L90
R90
F68
N2
W5
N4
W3
L90
E1
W1
L180
R90
F45
E5
R90
W5
S4
F5
L180
N1
R90
S4
E3
F22
R180
W4
L180
S3
L90
N5
E5
N1
F6
S5
W1
F86
R180
S1
R90
E5
N2
L90
W4
N1
W3
R90
F1
R180
F94
L90
E5
F7
R90
F72
R90
N3
N1
L180
N4
L90
N5
E1
N1
L270
S2
R90
F8
N4
E2
F8
S5
E2
S3
L90
F67
E4
F54
E1
F100
N2
F20
"""

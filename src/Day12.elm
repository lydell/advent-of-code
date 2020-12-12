module Day12 exposing (..)

import Day12Input exposing (puzzleInput)
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

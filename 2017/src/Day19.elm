module Day19 exposing (..)

import Array
import Char
import Day19Input exposing (input)
import List.Extra
import Matrix exposing (Matrix)


output : () -> ( String, String )
output () =
    ( input |> parse |> goThrough |> Tuple.second |> List.map String.fromChar |> String.join ""
    , input |> parse |> goThrough |> Tuple.first |> toString
    )


type Instruction
    = Empty
    | MoveVertical
    | MoveHorizontal
    | Letter Char
    | Turn


type Direction
    = Up
    | Down
    | Left
    | Right


type DirectionType
    = Vertical
    | Horizontal


codeA : Int
codeA =
    Char.toCode 'A'


codeZ : Int
codeZ =
    Char.toCode 'Z'


padListRight : Int -> a -> List a -> List a
padListRight n fill list =
    let
        diff =
            n - List.length list
    in
    if diff > 0 then
        list ++ List.repeat diff fill

    else
        list


parse : String -> Matrix Instruction
parse string =
    let
        lines =
            string
                |> String.lines
                |> List.map parseLine

        longest =
            lines |> List.map List.length |> List.maximum |> Maybe.withDefault 0

        paddedLines =
            List.map (padListRight longest Empty) lines
    in
    Matrix.fromList paddedLines
        |> Maybe.withDefault Matrix.empty


parseLine : String -> List Instruction
parseLine =
    String.toList >> List.filterMap parseChar


parseChar : Char -> Maybe Instruction
parseChar char =
    case char of
        ' ' ->
            Just Empty

        '|' ->
            Just MoveVertical

        '-' ->
            Just MoveHorizontal

        '+' ->
            Just Turn

        _ ->
            let
                code =
                    Char.toCode char
            in
            if code >= codeA && code <= codeZ then
                Just (Letter char)

            else
                Nothing


step : Matrix Instruction -> ( Int, Int, Direction ) -> ( Int, Int, Direction )
step grid ( x, y, direction ) =
    case Matrix.get x y grid of
        Just MoveVertical ->
            withDirection direction (moveAlong ( x, y, direction ))

        Just MoveHorizontal ->
            withDirection direction (moveAlong ( x, y, direction ))

        Just (Letter _) ->
            withDirection direction (moveAlong ( x, y, direction ))

        Just Turn ->
            let
                ( directionA, directionB ) =
                    case directionType direction of
                        Vertical ->
                            ( Left, Right )

                        Horizontal ->
                            ( Up, Down )

                ( coordsA, coordsB ) =
                    ( moveAlong ( x, y, directionA )
                    , moveAlong ( x, y, directionB )
                    )

                get ( x2, y2 ) =
                    case Matrix.get x2 y2 grid of
                        Just instruction ->
                            canTurnTo instruction direction

                        Nothing ->
                            False
            in
            case ( get coordsA, get coordsB ) of
                ( True, True ) ->
                    let
                        _ =
                            Debug.log "Got two possible turns at" ( x, y, direction )
                    in
                    withDirection directionA coordsA

                ( True, False ) ->
                    withDirection directionA coordsA

                ( False, True ) ->
                    withDirection directionB coordsB

                ( False, False ) ->
                    let
                        _ =
                            Debug.log "Got no possible turns at" ( x, y, direction )
                    in
                    ( -1, -1, direction )

        Just Empty ->
            ( x, y, direction )

        Nothing ->
            ( x, y, direction )


directionType : Direction -> DirectionType
directionType direction =
    case direction of
        Up ->
            Vertical

        Down ->
            Vertical

        Left ->
            Horizontal

        Right ->
            Horizontal


canTurnTo : Instruction -> Direction -> Bool
canTurnTo instruction direction =
    case instruction of
        Empty ->
            False

        MoveVertical ->
            case directionType direction of
                Vertical ->
                    False

                Horizontal ->
                    True

        MoveHorizontal ->
            case directionType direction of
                Vertical ->
                    True

                Horizontal ->
                    False

        Letter _ ->
            True

        Turn ->
            True


moveAlong : ( Int, Int, Direction ) -> ( Int, Int )
moveAlong ( x, y, direction ) =
    case direction of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


withDirection : Direction -> ( Int, Int ) -> ( Int, Int, Direction )
withDirection direction ( x, y ) =
    ( x, y, direction )


goThrough : Matrix Instruction -> ( Int, List Char )
goThrough grid =
    Matrix.getRow 0 grid
        |> Maybe.andThen (Array.toList >> List.Extra.elemIndex MoveVertical)
        |> Maybe.map (\x -> goThroughHelper grid ( x, 0, Down ) ( 0, [] ))
        |> Maybe.map (Tuple.mapSecond List.reverse)
        |> Maybe.withDefault ( 0, [] )


goThroughHelper :
    Matrix Instruction
    -> ( Int, Int, Direction )
    -> ( Int, List Char )
    -> ( Int, List Char )
goThroughHelper grid ( x, y, direction ) ( numSteps, chars ) =
    case Matrix.get x y grid of
        Nothing ->
            ( numSteps, chars )

        Just Empty ->
            ( numSteps, chars )

        Just (Letter char) ->
            goThroughHelper
                grid
                (step grid ( x, y, direction ))
                ( numSteps + 1, char :: chars )

        Just _ ->
            goThroughHelper
                grid
                (step grid ( x, y, direction ))
                ( numSteps + 1, chars )

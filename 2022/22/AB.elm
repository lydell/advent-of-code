module AB exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Input
import Process
import Set exposing (Set)
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
    { map : Dict ( Int, Int ) Item
    , cube : Dict ( Int, Int, Int ) ItemWithData
    , bounds : Bounds
    , rowBounds : Dict Int ( Int, Int )
    , columnBounds : Dict Int ( Int, Int )
    , directionsMap : Dict ( Int, Int, Int ) (Dict ( Int, Int, Int ) Direction)
    , sideLength : Int
    , moves : List Move
    , positionA : ( Int, Int )
    , positionB : ( Int, Int, Int )
    , directionA : Direction
    , directionB : ( Int, Int, Int )
    , historyA : List ( ( Int, Int ), Direction )
    , historyB : List ( ( Int, Int, Int ), ( Int, Int, Int ) )
    , playing : Bool
    , steppingToEnd : Bool
    , part : Part
    , fps : Int
    , fpsInput : String
    , input : Input
    , inputError : Maybe String
    }


type Item
    = Wall
    | Floor


type alias ItemWithData =
    { item : Item
    , originalCoordinate : ( Int, Int )
    }


type Move
    = GoForward Int
    | TurnLeft
    | TurnRight


type Direction
    = Up
    | Down
    | Left
    | Right


type Part
    = A
    | B


type Input
    = Example
    | Full


type Msg
    = Step
    | PlayPauseToggled
    | StepToEndClicked
    | StepToEndStarted
    | SwitchPartClicked Part
    | SwitchInputClicked Input
    | FpsInputChanged String


init : () -> ( Model, Cmd Msg )
init () =
    let
        fps =
            8
    in
    ( initPart
        { map = Dict.empty
        , cube = Dict.empty
        , bounds = mapToBounds Dict.empty
        , rowBounds = Dict.empty
        , columnBounds = Dict.empty
        , directionsMap = Dict.empty
        , sideLength = 0
        , moves = []
        , positionA = ( 0, 0 )
        , positionB = ( 0, 0, 0 )
        , directionA = Right
        , directionB = ( 0, 0, 0 )
        , historyA = []
        , historyB = []
        , playing = False
        , steppingToEnd = False
        , part = A
        , fps = fps
        , fpsInput = String.fromInt fps
        , input = Example
        , inputError = Nothing
        }
    , Cmd.none
    )


initPart : Model -> Model
initPart model =
    let
        input =
            case model.input of
                Example ->
                    Input.example

                Full ->
                    Input.input
    in
    case parse input of
        Ok ( map, moves ) ->
            let
                bounds =
                    mapToBounds map

                xRange =
                    List.range bounds.xMin bounds.xMax

                yRange =
                    List.range bounds.yMin bounds.yMax

                rowBounds =
                    yRange
                        |> List.map
                            (\y ->
                                let
                                    xs =
                                        xRange
                                            |> List.filterMap
                                                (\x ->
                                                    if Dict.member ( x, y ) map then
                                                        Just x

                                                    else
                                                        Nothing
                                                )
                                in
                                ( y, ( minimum xs, maximum xs ) )
                            )
                        |> Dict.fromList

                columnBounds =
                    xRange
                        |> List.map
                            (\x ->
                                let
                                    ys =
                                        yRange
                                            |> List.filterMap
                                                (\y ->
                                                    if Dict.member ( x, y ) map then
                                                        Just y

                                                    else
                                                        Nothing
                                                )
                                in
                                ( x, ( minimum ys, maximum ys ) )
                            )
                        |> Dict.fromList

                sideLength =
                    getSideLength bounds

                start =
                    findStart sideLength 0 bounds map

                chain =
                    makeChain sideLength start (Set.singleton start) map

                hardcoded =
                    case model.input of
                        Example ->
                            exampleHardcoded

                        Full ->
                            fullHardcoded

                { cube, directionsMap } =
                    walkChain sideLength hardcoded [] chain
            in
            case Dict.get bounds.yMin rowBounds of
                Just ( xMin, _ ) ->
                    { model
                        | map = map
                        , cube = cube
                        , bounds = bounds
                        , rowBounds = rowBounds
                        , columnBounds = columnBounds
                        , directionsMap = directionsMap
                        , sideLength = sideLength
                        , moves = moves
                        , positionA = ( xMin, bounds.yMin )
                        , positionB = ( 0, sideLength - 1, sideLength )
                        , directionA = Right
                        , directionB = ( 1, 0, 0 )
                        , historyA = []
                        , historyB = []
                    }

                Nothing ->
                    { model | inputError = Just "No starting position found." }

        Err errors ->
            { model | inputError = Just (String.join "\n" errors) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            ( (case model.part of
                A ->
                    stepA

                B ->
                    stepB
              )
                model
            , Cmd.none
            )

        PlayPauseToggled ->
            ( { model | playing = not model.playing }, Cmd.none )

        StepToEndClicked ->
            ( { model | steppingToEnd = True, playing = False }
            , Process.sleep 50 |> Task.perform (always StepToEndStarted)
            )

        StepToEndStarted ->
            let
                nextModel =
                    (case model.part of
                        A ->
                            stepToEndA

                        B ->
                            stepToEndB
                    )
                        { model | playing = True }
            in
            ( { nextModel | steppingToEnd = False }, Cmd.none )

        SwitchPartClicked part ->
            ( initPart { model | part = part }, Cmd.none )

        SwitchInputClicked input ->
            ( initPart { model | input = input }, Cmd.none )

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


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing then
        Time.every (1000 / toFloat model.fps) (always Step)

    else
        Sub.none


parse : String -> Result (List String) ( Dict ( Int, Int ) Item, List Move )
parse input =
    case input |> trimLeadingNewlines |> String.trimRight |> String.split "\n\n" of
        [ first, second ] ->
            let
                mapResult =
                    first
                        |> String.split "\n"
                        |> List.indexedMap
                            (\lineIndex line ->
                                line
                                    |> String.toList
                                    |> List.indexedMap
                                        (\charIndex char ->
                                            case char of
                                                ' ' ->
                                                    Ok Nothing

                                                '.' ->
                                                    Ok (Just ( ( charIndex, lineIndex ), Floor ))

                                                '#' ->
                                                    Ok (Just ( ( charIndex, lineIndex ), Wall ))

                                                _ ->
                                                    Err ("Line " ++ String.fromInt (lineIndex + 1) ++ ", char " ++ String.fromInt (charIndex + 1) ++ ": Invalid char: " ++ String.fromChar char)
                                        )
                                    |> collectResults
                                    |> Result.map (List.filterMap identity)
                            )
                        |> collectResults
                        |> Result.map (List.concat >> Dict.fromList)
                        |> Result.mapError List.concat

                movesResult =
                    parseMoves (String.toList second) 0
            in
            case ( mapResult, movesResult ) of
                ( Ok map, Ok moves ) ->
                    Ok ( map, moves )

                ( Err mapErrors, Err moveError ) ->
                    Err (mapErrors ++ [ moveError ])

                ( Err mapErrors, Ok _ ) ->
                    Err mapErrors

                ( Ok _, Err moveError ) ->
                    Err [ moveError ]

        list ->
            Err [ "Expected two sections separated by a blank line, but got: " ++ String.fromInt (List.length list) ++ " sections." ]


trimLeadingNewlines : String -> String
trimLeadingNewlines string =
    case String.uncons string of
        Just ( '\n', rest ) ->
            trimLeadingNewlines rest

        _ ->
            string


parseMoves : List Char -> Int -> Result String (List Move)
parseMoves chars buffer =
    case chars of
        [] ->
            Ok [ GoForward buffer ]

        char :: rest ->
            case char of
                'L' ->
                    parseMoves rest 0
                        |> Result.map (\moves -> GoForward buffer :: TurnLeft :: moves)

                'R' ->
                    parseMoves rest 0
                        |> Result.map (\moves -> GoForward buffer :: TurnRight :: moves)

                _ ->
                    if Char.isDigit char then
                        case String.toInt (String.fromChar char) of
                            Just int ->
                                parseMoves rest (buffer * 10 + int)

                            Nothing ->
                                Err ("Invalid char: " ++ String.fromChar char)

                    else
                        Err ("Invalid move char: " ++ String.fromChar char)


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


type alias Bounds =
    { xMin : Int
    , xMax : Int
    , yMin : Int
    , yMax : Int
    }


mapToBounds : Dict ( Int, Int ) Item -> Bounds
mapToBounds map =
    let
        ( xs, ys ) =
            Dict.keys map
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


getSideLength : Bounds -> Int
getSideLength bounds =
    gcd (bounds.xMax - bounds.xMin + 1) (bounds.yMax - bounds.yMin + 1)


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (a |> modBy b)


findStart : Int -> Int -> Bounds -> Dict ( Int, Int ) Item -> ( Int, Int )
findStart sideLength x bounds map =
    case Dict.get ( x, bounds.yMin ) map of
        Just _ ->
            ( x, bounds.yMin )

        Nothing ->
            findStart sideLength (x + sideLength) bounds map


type Chain
    = Chain
        { map : Dict ( Int, Int ) ItemWithData
        , links : List ( Direction, Chain )
        }


makeChain : Int -> ( Int, Int ) -> Set ( Int, Int ) -> Dict ( Int, Int ) Item -> Chain
makeChain sideLength ( x, y ) visited map =
    let
        mapPiece =
            List.range x (x + sideLength - 1)
                |> List.concatMap
                    (\tx ->
                        List.range y (y + sideLength - 1)
                            |> List.map
                                (\ty ->
                                    Dict.get ( tx, ty ) map
                                        |> Maybe.map
                                            (\item ->
                                                ( ( tx - x, sideLength - 1 - ty + y )
                                                , { item = item
                                                  , originalCoordinate = ( tx, ty )
                                                  }
                                                )
                                            )
                                )
                    )
                |> List.filterMap identity
                |> Dict.fromList

        try direction nextCoordinate =
            if Set.member nextCoordinate visited then
                Nothing

            else if Dict.member nextCoordinate map then
                Just ( direction, makeChain sideLength nextCoordinate (Set.insert nextCoordinate visited) map )

            else
                Nothing
    in
    Chain
        { map = mapPiece
        , links =
            [ try Left ( x - sideLength, y )
            , try Right ( x + sideLength, y )
            , try Up ( x, y - sideLength )
            , try Down ( x, y + sideLength )
            ]
                |> List.filterMap identity
        }


{-| I couldn’t figure out how to do the folding in general, so I hardcoded some
stuff for the example and the full input.
-}
type alias Hardcoded =
    Int
    -> List Direction
    ->
        { transform : Dict ( Int, Int ) ItemWithData -> Dict ( Int, Int, Int ) ItemWithData
        , directions : Dict ( Int, Int, Int ) Direction
        }


exampleHardcoded : Hardcoded
exampleHardcoded sideLength path =
    case path of
        [] ->
            { transform =
                mapKeys (\( x, y ) -> ( x, y, sideLength ))
            , directions =
                Dict.fromList
                    [ ( ( 0, 1, 0 ), Up )
                    , ( ( 0, -1, 0 ), Down )
                    , ( ( -1, 0, 0 ), Left )
                    , ( ( 1, 0, 0 ), Right )
                    ]
            }

        [ Down ] ->
            { transform =
                mapKeys (\( x, y ) -> ( x, -1, y ))
            , directions =
                Dict.fromList
                    [ ( ( 0, 0, 1 ), Up )
                    , ( ( 0, 0, -1 ), Down )
                    , ( ( -1, 0, 0 ), Left )
                    , ( ( 1, 0, 0 ), Right )
                    ]
            }

        [ Down, Left ] ->
            { transform =
                mapKeys (\( x, y ) -> ( -1, sideLength - 1 - x, y ))
            , directions =
                Dict.fromList
                    [ ( ( 0, 0, 1 ), Up )
                    , ( ( 0, 0, -1 ), Down )
                    , ( ( 0, 1, 0 ), Left )
                    , ( ( 0, -1, 0 ), Right )
                    ]
            }

        [ Down, Left, Left ] ->
            { transform =
                mapKeys (\( x, y ) -> ( sideLength - 1 - x, sideLength, y ))
            , directions =
                Dict.fromList
                    [ ( ( 0, 0, 1 ), Up )
                    , ( ( 0, 0, -1 ), Down )
                    , ( ( 1, 0, 0 ), Left )
                    , ( ( -1, 0, 0 ), Right )
                    ]
            }

        [ Down, Down ] ->
            { transform =
                mapKeys (\( x, y ) -> ( x, sideLength - 1 - y, -1 ))
            , directions =
                Dict.fromList
                    [ ( ( 0, -1, 0 ), Up )
                    , ( ( 0, 1, 0 ), Down )
                    , ( ( -1, 0, 0 ), Left )
                    , ( ( 1, 0, 0 ), Right )
                    ]
            }

        [ Down, Down, Right ] ->
            { transform =
                mapKeys (\( x, y ) -> ( sideLength, sideLength - 1 - y, x ))
            , directions =
                Dict.fromList
                    [ ( ( 0, -1, 0 ), Up )
                    , ( ( 0, 1, 0 ), Down )
                    , ( ( 0, 0, -1 ), Left )
                    , ( ( 0, 0, 1 ), Right )
                    ]
            }

        _ ->
            Debug.todo ("exampleHardcoded: Unsupported path: " ++ Debug.toString path)


fullHardcoded : Hardcoded
fullHardcoded sideLength path =
    case path of
        [] ->
            { transform =
                mapKeys (\( x, y ) -> ( x, y, sideLength ))
            , directions =
                Dict.fromList
                    [ ( ( 0, 1, 0 ), Up )
                    , ( ( 0, -1, 0 ), Down )
                    , ( ( -1, 0, 0 ), Left )
                    , ( ( 1, 0, 0 ), Right )
                    ]
            }

        [ Right ] ->
            { transform =
                mapKeys (\( x, y ) -> ( sideLength, y, sideLength - 1 - x ))
            , directions =
                Dict.fromList
                    [ ( ( 0, 1, 0 ), Up )
                    , ( ( 0, -1, 0 ), Down )
                    , ( ( 0, 0, 1 ), Left )
                    , ( ( 0, 0, -1 ), Right )
                    ]
            }

        [ Down ] ->
            { transform =
                mapKeys (\( x, y ) -> ( x, -1, y ))
            , directions =
                Dict.fromList
                    [ ( ( 0, 0, 1 ), Up )
                    , ( ( 0, 0, -1 ), Down )
                    , ( ( -1, 0, 0 ), Left )
                    , ( ( 1, 0, 0 ), Right )
                    ]
            }

        [ Down, Down ] ->
            { transform =
                mapKeys (\( x, y ) -> ( x, sideLength - 1 - y, -1 ))
            , directions =
                Dict.fromList
                    [ ( ( 0, -1, 0 ), Up )
                    , ( ( 0, 1, 0 ), Down )
                    , ( ( -1, 0, 0 ), Left )
                    , ( ( 1, 0, 0 ), Right )
                    ]
            }

        [ Down, Down, Left ] ->
            { transform =
                mapKeys (\( x, y ) -> ( -1, sideLength - 1 - y, sideLength - 1 - x ))
            , directions =
                Dict.fromList
                    [ ( ( 0, -1, 0 ), Up )
                    , ( ( 0, 1, 0 ), Down )
                    , ( ( 0, 0, 1 ), Left )
                    , ( ( 0, 0, -1 ), Right )
                    ]
            }

        [ Down, Down, Left, Down ] ->
            { transform =
                mapKeys (\( x, y ) -> ( sideLength - 1 - y, sideLength, sideLength - 1 - x ))
            , directions =
                Dict.fromList
                    [ ( ( -1, 0, 0 ), Up )
                    , ( ( 1, 0, 0 ), Down )
                    , ( ( 0, 0, 1 ), Left )
                    , ( ( 0, 0, -1 ), Right )
                    ]
            }

        _ ->
            Debug.todo ("fullHardcoded: Unsupported path: " ++ Debug.toString path)


mapKeys : (comparable1 -> comparable2) -> Dict comparable1 v -> Dict comparable2 v
mapKeys f dict =
    Dict.toList dict
        |> List.map (\( key, value ) -> ( f key, value ))
        |> Dict.fromList


walkChain :
    Int
    -> Hardcoded
    -> List Direction
    -> Chain
    ->
        { cube : Dict ( Int, Int, Int ) ItemWithData
        , directionsMap : Dict ( Int, Int, Int ) (Dict ( Int, Int, Int ) Direction)
        }
walkChain sideLength hardcoded path (Chain chain) =
    let
        { transform, directions } =
            hardcoded sideLength path

        cube =
            transform chain.map

        onePosition =
            case Dict.toList cube of
                ( first, _ ) :: _ ->
                    first

                [] ->
                    Debug.todo "walkChain: Empty cube"

        directionKey =
            getDirectionKey sideLength onePosition
    in
    chain.links
        |> List.foldl
            (\( direction, nextChain ) acc ->
                let
                    next =
                        walkChain sideLength hardcoded (path ++ [ direction ]) nextChain
                in
                { cube = Dict.union acc.cube next.cube
                , directionsMap = Dict.union acc.directionsMap next.directionsMap
                }
            )
            { cube = cube
            , directionsMap = Dict.singleton directionKey directions
            }


getDirectionKey : Int -> ( Int, Int, Int ) -> ( Int, Int, Int )
getDirectionKey sideLength ( x, y, z ) =
    if x < 0 || x >= sideLength then
        ( x, 0, 0 )

    else if y < 0 || y >= sideLength then
        ( 0, y, 0 )

    else if z < 0 || z >= sideLength then
        ( 0, 0, z )

    else
        Debug.todo ("directionKey: Not found for: " ++ Debug.toString ( x, y, z ))


stepA : Model -> Model
stepA model =
    case model.moves of
        [] ->
            { model | playing = False }

        move :: rest ->
            case move of
                GoForward n ->
                    let
                        ( directionChange, warp ) =
                            directionToMover model.rowBounds model.columnBounds model.directionA

                        ( newPosition, newN ) =
                            doMoveA warp model.positionA directionChange n model.map
                    in
                    { model
                        | positionA = newPosition
                        , historyA =
                            if newPosition == model.positionA then
                                model.historyA

                            else
                                model.historyA ++ [ ( model.positionA, model.directionA ) ]
                        , moves =
                            if newN > 0 then
                                GoForward newN :: rest

                            else
                                rest
                    }

                TurnLeft ->
                    { model
                        | directionA = turnLeftA model.directionA
                        , moves = rest
                    }

                TurnRight ->
                    { model
                        | directionA = turnRightA model.directionA
                        , moves = rest
                    }


stepB : Model -> Model
stepB model =
    case model.moves of
        [] ->
            { model | playing = False }

        move :: rest ->
            case move of
                GoForward n ->
                    let
                        ( moved, newN ) =
                            doMoveB n model
                    in
                    { moved
                        | historyB =
                            if moved.positionB == model.positionB then
                                model.historyB

                            else
                                model.historyB ++ [ ( model.positionB, model.directionB ) ]
                        , moves =
                            if newN > 0 then
                                GoForward newN :: rest

                            else
                                rest
                    }

                TurnLeft ->
                    { model
                        | directionB = turnLeftB (getDirectionKey model.sideLength model.positionB) model.directionB
                        , moves = rest
                    }

                TurnRight ->
                    { model
                        | directionB = turnRightB (getDirectionKey model.sideLength model.positionB) model.directionB
                        , moves = rest
                    }


stepToEndA : Model -> Model
stepToEndA model =
    let
        nextModel =
            stepA model
    in
    if nextModel.playing then
        stepToEndA nextModel

    else
        nextModel


stepToEndB : Model -> Model
stepToEndB model =
    let
        nextModel =
            stepB model
    in
    if nextModel.playing then
        stepToEndB nextModel

    else
        nextModel


moveToString : Move -> String
moveToString move =
    case move of
        GoForward n ->
            "🚶\u{200D}♂️" ++ String.fromInt n

        TurnLeft ->
            "👈"

        TurnRight ->
            "👉"


itemToColor : Item -> String
itemToColor item =
    case item of
        Floor ->
            "rgba(0, 0, 255, 0.5)"

        Wall ->
            "white"


turnLeftA : Direction -> Direction
turnLeftA direction =
    case direction of
        Up ->
            Left

        Down ->
            Right

        Left ->
            Down

        Right ->
            Up


turnRightA : Direction -> Direction
turnRightA direction =
    case direction of
        Up ->
            Right

        Down ->
            Left

        Left ->
            Up

        Right ->
            Down


turnLeftB : ( Int, Int, Int ) -> ( Int, Int, Int ) -> ( Int, Int, Int )
turnLeftB directionKey direction =
    case directionKey of
        ( 0, 0, z ) ->
            case directionToOrder direction of
                ( LT, EQ, _ ) ->
                    ( 0, -(sign z), 0 )

                ( GT, EQ, _ ) ->
                    ( 0, sign z, 0 )

                ( EQ, LT, _ ) ->
                    ( sign z, 0, 0 )

                ( EQ, GT, _ ) ->
                    ( -(sign z), 0, 0 )

                ( _, _, _ ) ->
                    Debug.todo ("turnLeftB: Unsupported direction: " ++ Debug.toString direction)

        ( 0, y, 0 ) ->
            case directionToOrder direction of
                ( LT, _, EQ ) ->
                    ( 0, 0, sign y )

                ( GT, _, EQ ) ->
                    ( 0, 0, -(sign y) )

                ( EQ, _, LT ) ->
                    ( -(sign y), 0, 0 )

                ( EQ, _, GT ) ->
                    ( sign y, 0, 0 )

                ( _, _, _ ) ->
                    Debug.todo ("turnLeftB: Unsupported direction: " ++ Debug.toString direction)

        ( x, 0, 0 ) ->
            case directionToOrder direction of
                ( _, LT, EQ ) ->
                    ( 0, 0, -(sign x) )

                ( _, GT, EQ ) ->
                    ( 0, 0, sign x )

                ( _, EQ, LT ) ->
                    ( 0, sign x, 0 )

                ( _, EQ, GT ) ->
                    ( 0, -(sign x), 0 )

                ( _, _, _ ) ->
                    Debug.todo ("turnLeftB: Unsupported direction: " ++ Debug.toString direction)

        ( _, _, _ ) ->
            Debug.todo ("turnLeftB: Unsupported directionKey: " ++ Debug.toString directionKey)


turnRightB : ( Int, Int, Int ) -> ( Int, Int, Int ) -> ( Int, Int, Int )
turnRightB directionKey =
    turnLeftB directionKey
        >> turnLeftB directionKey
        >> turnLeftB directionKey


sign : number -> number
sign x =
    if x < 0 then
        -1

    else if x > 0 then
        1

    else
        Debug.todo "sign: Got 0"


directionToOrder : ( Int, Int, Int ) -> ( Order, Order, Order )
directionToOrder ( x, y, z ) =
    ( intToOrder x, intToOrder y, intToOrder z )


intToOrder : Int -> Order
intToOrder n =
    if n < 0 then
        LT

    else if n > 0 then
        GT

    else
        EQ


directionToString : Direction -> String
directionToString direction =
    case direction of
        Up ->
            "⬆️"

        Down ->
            "⬇️"

        Left ->
            "⬅️"

        Right ->
            "➡️"


directionToFacing : Direction -> Int
directionToFacing direction =
    case direction of
        Up ->
            3

        Down ->
            1

        Left ->
            2

        Right ->
            0


directionToMover : Dict Int ( Int, Int ) -> Dict Int ( Int, Int ) -> Direction -> ( ( Int, Int ), ( Int, Int ) -> ( Int, Int ) )
directionToMover rowBounds columnBounds direction =
    case direction of
        Up ->
            ( ( 0, -1 )
            , \( x, _ ) ->
                case Dict.get x columnBounds of
                    Just ( _, yMax ) ->
                        ( x, yMax )

                    Nothing ->
                        Debug.todo ("Up: x not found in columnBounds: " ++ String.fromInt x)
            )

        Down ->
            ( ( 0, 1 )
            , \( x, _ ) ->
                case Dict.get x columnBounds of
                    Just ( yMin, _ ) ->
                        ( x, yMin )

                    Nothing ->
                        Debug.todo ("Down: x not found in columnBounds: " ++ String.fromInt x)
            )

        Left ->
            ( ( -1, 0 )
            , \( _, y ) ->
                case Dict.get y rowBounds of
                    Just ( _, xMax ) ->
                        ( xMax, y )

                    Nothing ->
                        Debug.todo ("Left: y not found in columnBounds: " ++ String.fromInt y)
            )

        Right ->
            ( ( 1, 0 )
            , \( _, y ) ->
                case Dict.get y rowBounds of
                    Just ( xMin, _ ) ->
                        ( xMin, y )

                    Nothing ->
                        Debug.todo ("Right: y not found in columnBounds: " ++ String.fromInt y)
            )


doMoveA : (( Int, Int ) -> ( Int, Int )) -> ( Int, Int ) -> ( Int, Int ) -> Int -> Dict ( Int, Int ) Item -> ( ( Int, Int ), Int )
doMoveA warp ( xStart, yStart ) ( dx, dy ) n map =
    if n <= 0 then
        ( ( xStart, yStart ), 0 )

    else
        let
            x =
                xStart + dx

            y =
                yStart + dy
        in
        case Dict.get ( x, y ) map of
            Nothing ->
                let
                    ( xWarp, yWarp ) =
                        warp ( x, y )
                in
                case Dict.get ( xWarp, yWarp ) map of
                    Nothing ->
                        Debug.todo ("warp failed: " ++ Debug.toString ( x, y ) ++ " -> " ++ Debug.toString ( xWarp, yWarp ))

                    Just Floor ->
                        ( ( xWarp, yWarp ), n - 1 )

                    Just Wall ->
                        ( ( xStart, yStart ), 0 )

            Just Floor ->
                ( ( x, y ), n - 1 )

            Just Wall ->
                ( ( xStart, yStart ), 0 )


doMoveB : Int -> Model -> ( Model, Int )
doMoveB n model =
    if n <= 0 then
        ( model, 0 )

    else
        let
            newPosition =
                move3d model.positionB model.directionB
        in
        case Dict.get newPosition model.cube |> Maybe.map .item of
            Nothing ->
                let
                    ( warpedPosition, warpedDirection ) =
                        warpB model.cube model.positionB newPosition
                in
                case Dict.get warpedPosition model.cube |> Maybe.map .item of
                    Nothing ->
                        Debug.todo ("warpB failed: " ++ Debug.toString model.positionB ++ " -> " ++ Debug.toString newPosition ++ " -> " ++ Debug.toString warpedPosition)

                    Just Floor ->
                        ( { model | positionB = warpedPosition, directionB = warpedDirection }, n - 1 )

                    Just Wall ->
                        ( model, 0 )

            Just Floor ->
                ( { model | positionB = newPosition }, n - 1 )

            Just Wall ->
                ( model, 0 )


move3d : ( Int, Int, Int ) -> ( Int, Int, Int ) -> ( Int, Int, Int )
move3d ( x, y, z ) ( dx, dy, dz ) =
    ( x + dx, y + dy, z + dz )


warpB : Dict ( Int, Int, Int ) ItemWithData -> ( Int, Int, Int ) -> ( Int, Int, Int ) -> ( ( Int, Int, Int ), ( Int, Int, Int ) )
warpB cube startPosition newPosition =
    let
        candidates =
            [ ( 0, 0, -1 )
            , ( 0, 0, 1 )
            , ( 0, -1, 0 )
            , ( 0, 1, 0 )
            , ( -1, 0, 0 )
            , ( 1, 0, 0 )
            ]
                |> List.filterMap
                    (\direction ->
                        let
                            position =
                                move3d newPosition direction
                        in
                        if Dict.member position cube && position /= startPosition then
                            Just ( position, direction )

                        else
                            Nothing
                    )
    in
    case candidates of
        [ candidate ] ->
            candidate

        _ ->
            Debug.todo ("warpB: Found too many candidates: " ++ Debug.toString candidates)


fpsMin : Int
fpsMin =
    1


getPositionAndDirectionBtoA : Model -> ( Int, Int, Int ) -> ( Int, Int, Int ) -> ( ( Int, Int ), Direction )
getPositionAndDirectionBtoA model positionB directionB =
    let
        positionA =
            case Dict.get positionB model.cube of
                Just item ->
                    item.originalCoordinate

                Nothing ->
                    Debug.todo ("positionB not found: " ++ Debug.toString positionB)

        directionKey =
            getDirectionKey model.sideLength positionB

        directionA =
            case Dict.get directionKey model.directionsMap of
                Just directions ->
                    case Dict.get directionB directions of
                        Just direction ->
                            direction

                        Nothing ->
                            Debug.todo ("directionB not found: " ++ Debug.toString model.directionB)

                Nothing ->
                    Debug.todo ("directionKey not found: " ++ Debug.toString directionKey)
    in
    ( positionA, directionA )


view : Model -> Html Msg
view model =
    let
        ( position, direction ) =
            case model.part of
                A ->
                    ( model.positionA, model.directionA )

                B ->
                    getPositionAndDirectionBtoA model model.positionB model.directionB
    in
    Html.div []
        [ viewSvg position direction model
        , viewControls position direction model
        ]


viewControls : ( Int, Int ) -> Direction -> Model -> Html Msg
viewControls ( x, y ) direction model =
    let
        row =
            y + 1

        column =
            x + 1

        facing =
            directionToFacing direction

        nextMoveString =
            case model.moves of
                [] ->
                    "(none)"

                move :: _ ->
                    moveToString move
    in
    Html.div [ Html.Attributes.id "controls-wrapper" ]
        ([ Html.div [ Html.Attributes.id "controls" ]
            [ Html.button [ Html.Events.onClick PlayPauseToggled, Html.Attributes.disabled (List.isEmpty model.moves) ]
                [ Html.text
                    (if model.playing then
                        "⏸"

                     else
                        "▶️"
                    )
                ]
            , Html.button [ Html.Events.onClick Step, Html.Attributes.disabled (List.isEmpty model.moves) ]
                [ Html.text "⏩" ]
            , if model.steppingToEnd then
                Html.button []
                    [ Html.text "⏳" ]

              else
                Html.button [ Html.Events.onClick StepToEndClicked, Html.Attributes.disabled (List.isEmpty model.moves) ]
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
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.name "input"
                    , Html.Events.onCheck (always (SwitchInputClicked Example))
                    , Html.Attributes.checked (model.input == Example)
                    ]
                    []
                , Html.text "Example"
                ]
            , Html.label []
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.name "input"
                    , Html.Events.onCheck (always (SwitchInputClicked Full))
                    , Html.Attributes.checked (model.input == Full)
                    ]
                    []
                , Html.text "Full input"
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
         , Html.div [] [ Html.text ("Row: " ++ String.fromInt row) ]
         , Html.div [] [ Html.text ("Column: " ++ String.fromInt column) ]
         , Html.div [] [ Html.text ("Direction: " ++ directionToString direction ++ " (" ++ String.fromInt facing ++ ")") ]
         , Html.div [] [ Html.text ("Next move: " ++ nextMoveString) ]
         , Html.div [] [ Html.text ("Solution: " ++ String.fromInt (1000 * row + 4 * column + facing)) ]
         , case model.inputError of
            Just error ->
                Html.pre [] [ Html.text error ]

            Nothing ->
                Html.text ""
         ]
            ++ (case model.part of
                    A ->
                        []

                    B ->
                        [ Html.div [] [ Html.text ("3D Position: " ++ Debug.toString model.positionB) ]
                        , Html.div [] [ Html.text ("3D Direction: " ++ Debug.toString model.directionB) ]
                        , Html.div [] [ Html.text ("3D Direction key: " ++ Debug.toString (getDirectionKey model.sideLength model.positionB)) ]
                        ]
               )
        )


viewSvg : ( Int, Int ) -> Direction -> Model -> Html Msg
viewSvg position direction model =
    let
        { bounds } =
            model

        padding =
            2

        viewBox =
            [ bounds.xMin - padding
            , bounds.yMin - padding
            , bounds.xMax - bounds.xMin + 2 * padding
            , bounds.yMax - bounds.yMin + 2 * padding
            ]
                |> List.map String.fromInt
                |> String.join " "
                |> Svg.Attributes.viewBox

        history =
            case model.part of
                A ->
                    model.historyA

                B ->
                    model.historyB
                        |> List.map (\( positionB, directionB ) -> getPositionAndDirectionBtoA model positionB directionB)
    in
    Svg.Keyed.node "svg"
        [ viewBox ]
        (( "viewMap", viewMap model.map )
            :: ( "viewPosition", viewPosition model.fps "magenta" position direction )
            :: List.indexedMap
                (\index ( position_, direction_ ) ->
                    ( "historyPosition" ++ String.fromInt index
                    , viewPosition model.fps "cyan" position_ direction_
                    )
                )
                history
        )


viewMap : Dict ( Int, Int ) Item -> Svg msg
viewMap map =
    Svg.g []
        (map
            |> Dict.toList
            |> List.map
                (\( ( x, y ), item ) ->
                    Svg.rect
                        [ Svg.Attributes.x (String.fromFloat (toFloat x - 0.5))
                        , Svg.Attributes.y (String.fromFloat (toFloat y - 0.5))
                        , Svg.Attributes.width "1"
                        , Svg.Attributes.height "1"
                        , Svg.Attributes.fill (itemToColor item)
                        ]
                        []
                )
        )


viewPosition : Int -> String -> ( Int, Int ) -> Direction -> Svg msg
viewPosition fps color ( x, y ) direction =
    let
        rotation =
            case direction of
                Up ->
                    90

                Down ->
                    270

                Left ->
                    0

                Right ->
                    180
    in
    Svg.path
        [ Svg.Attributes.d "m-0.5,0 l1,-0.5 l0,1 z"
        , Svg.Attributes.fill color
        , Svg.Attributes.transform ("translate(" ++ String.fromInt x ++ " " ++ String.fromInt y ++ ") rotate(" ++ String.fromInt rotation ++ ")")
        , Svg.Attributes.style ("transition: transform " ++ String.fromFloat (1000 / toFloat fps) ++ "ms ease")
        ]
        []

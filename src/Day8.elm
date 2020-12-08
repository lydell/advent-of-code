module Day8 exposing (..)

import Array exposing (Array)
import Day2 exposing (solution1)
import Day8Input exposing (puzzleInput)
import Html exposing (Html)
import LineParser
import Set exposing (Set)


type Instruction
    = Acc Int
    | Jmp Int
    | Nop Int


parse : String -> Result String (Array Instruction)
parse =
    LineParser.parse
        (\line ->
            case String.words line of
                [ "acc", arg ] ->
                    withInt Acc arg

                [ "jmp", arg ] ->
                    withInt Jmp arg

                [ "nop", arg ] ->
                    withInt Nop arg

                _ ->
                    Err "Unknown instruction"
        )
        >> Result.map Array.fromList


withInt : (Int -> Instruction) -> String -> Result String Instruction
withInt constructor arg =
    case String.toInt arg of
        Just int ->
            Ok (constructor int)

        Nothing ->
            Err ("Expected an int but got: " ++ arg)


type alias State =
    { acc : Int
    , index : Int
    , executed : Set Int
    }


emptyState : State
emptyState =
    { acc = 0
    , index = 0
    , executed = Set.empty
    }


solution1 : String -> Result String Int
solution1 =
    parse >> Result.andThen (solution1Helper emptyState)


solution1Helper : State -> Array Instruction -> Result String Int
solution1Helper state instructions =
    let
        ( stepResult, nextState ) =
            step instructions state
    in
    case stepResult of
        Success ->
            solution1Helper nextState instructions

        NoInstruction ->
            Err
                ("NoInstruction. Length: "
                    ++ String.fromInt (Array.length instructions)
                    ++ ". Index: "
                    ++ String.fromInt state.index
                )

        LoopDetected ->
            Ok state.acc


type StepResult
    = Success
    | NoInstruction
    | LoopDetected


step : Array Instruction -> State -> ( StepResult, State )
step instructions state =
    case Array.get state.index instructions of
        Just instruction ->
            if Set.member state.index state.executed then
                ( LoopDetected, state )

            else
                let
                    nextState =
                        case instruction of
                            Acc int ->
                                { state
                                    | index = state.index + 1
                                    , acc = state.acc + int
                                }

                            Jmp int ->
                                { state | index = state.index + int }

                            Nop _ ->
                                { state | index = state.index + 1 }
                in
                ( Success
                , { nextState | executed = Set.insert state.index nextState.executed }
                )

        Nothing ->
            ( NoInstruction, state )


solution2 : String -> Result String Int
solution2 =
    parse >> Result.andThen (solution2Helper emptyState << makeVariations)


solution2Helper : State -> List (Array Instruction) -> Result String Int
solution2Helper state instructionsVariations =
    case instructionsVariations of
        [] ->
            Err "No terminating variation found."

        first :: rest ->
            let
                ( stepResult, nextState ) =
                    step first state
            in
            case stepResult of
                Success ->
                    solution2Helper nextState instructionsVariations

                NoInstruction ->
                    Ok state.acc

                LoopDetected ->
                    solution2Helper emptyState rest


makeVariations : Array Instruction -> List (Array Instruction)
makeVariations instructions =
    makeVariationsHelper instructions 0 []


makeVariationsHelper : Array Instruction -> Int -> List (Array Instruction) -> List (Array Instruction)
makeVariationsHelper instructions index variations =
    case Array.get index instructions of
        Just instruction ->
            case instruction of
                Acc _ ->
                    makeVariationsHelper instructions
                        (index + 1)
                        variations

                Jmp int ->
                    makeVariationsHelper instructions
                        (index + 1)
                        (Array.set index (Nop int) instructions :: variations)

                Nop int ->
                    makeVariationsHelper instructions
                        (index + 1)
                        (Array.set index (Jmp int) instructions :: variations)

        Nothing ->
            variations


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
        , showResult (solution2 puzzleInput)
        ]


showResult : Result String Int -> Html msg
showResult result =
    Html.output []
        [ Html.text
            (case result of
                Ok int ->
                    String.fromInt int

                Err error ->
                    error
            )
        ]

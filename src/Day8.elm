module Day8 exposing (..)

import Day8Input exposing (input)
import Dict exposing (Dict)
import Regex exposing (HowMany(AtMost), Match, Regex)


output : () -> ( String, String )
output () =
    ( input |> parse |> evaluate |> largestRegisterValue |> toString
    , input |> parse |> evaluate |> .largestValue |> toString
    )


type alias Instruction =
    { register : String
    , changeType : ChangeType
    , changeValue : Int
    , condition : Condition
    }


type ChangeType
    = Inc
    | Dec


type alias Condition =
    { operator : Operator
    , register : String
    , value : Int
    }


type Operator
    = O_LT
    | O_LTE
    | O_GT
    | O_GTE
    | O_EQ
    | O_NEQ


type alias Registry =
    { registers : Dict String Int
    , largestValue : Int
    }


lineRegex : Regex
lineRegex =
    Regex.regex "^([a-z]+) ([a-z]+) (-?\\d+) if ([a-z]+) ([=<>!]+) (-?\\d+)$"


parse : String -> List Instruction
parse string =
    string
        |> String.lines
        |> List.filterMap parseLine


parseLine : String -> Maybe Instruction
parseLine string =
    string
        |> Regex.find (AtMost 1) lineRegex
        |> List.head
        |> Maybe.andThen parseMatch


parseMatch : Match -> Maybe Instruction
parseMatch { submatches } =
    case submatches of
        [ Just register, Just changeTypeString, Just changeValueString, Just conditionRegister, Just operatorString, Just conditionValueString ] ->
            let
                parsed =
                    ( parseChangeType changeTypeString
                    , String.toInt changeValueString
                    , parseOperator operatorString
                    , String.toInt conditionValueString
                    )
            in
            case parsed of
                ( Ok changeType, Ok changeValue, Ok operator, Ok conditionValue ) ->
                    Just
                        { register = register
                        , changeType = changeType
                        , changeValue = changeValue
                        , condition =
                            { operator = operator
                            , register = conditionRegister
                            , value = conditionValue
                            }
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


parseChangeType : String -> Result String ChangeType
parseChangeType string =
    case string of
        "inc" ->
            Ok Inc

        "dec" ->
            Ok Dec

        _ ->
            Err ("Invalid change type: " ++ string)


parseOperator : String -> Result String Operator
parseOperator string =
    case string of
        "<" ->
            Ok O_LT

        "<=" ->
            Ok O_LTE

        ">" ->
            Ok O_GT

        ">=" ->
            Ok O_GTE

        "==" ->
            Ok O_EQ

        "!=" ->
            Ok O_NEQ

        _ ->
            Err ("Invalid operator: " ++ string)


evaluate : List Instruction -> Registry
evaluate instructions =
    List.foldl evaluateInstruction { registers = Dict.empty, largestValue = 0 } instructions


evaluateInstruction : Instruction -> Registry -> Registry
evaluateInstruction instruction registry =
    let
        { registers, largestValue } =
            registry

        sign =
            case instruction.changeType of
                Inc ->
                    1

                Dec ->
                    -1

        increment =
            instruction.changeValue * sign

        shouldUpdate =
            evaluateCondition instruction.condition registers

        oldValue =
            Dict.get instruction.register registers
                |> Maybe.withDefault 0

        newValue =
            oldValue + increment
    in
    if shouldUpdate then
        { registers = Dict.insert instruction.register newValue registers
        , largestValue = max largestValue newValue
        }

    else
        registry


evaluateCondition : Condition -> Dict String Int -> Bool
evaluateCondition condition registers =
    let
        registerValue =
            Dict.get condition.register registers
                |> Maybe.withDefault 0

        operatorFunction =
            getOperatorFunction condition.operator
    in
    operatorFunction registerValue condition.value


getOperatorFunction : Operator -> (comparable -> comparable -> Bool)
getOperatorFunction operator =
    case operator of
        O_LT ->
            (<)

        O_LTE ->
            (<=)

        O_GT ->
            (>)

        O_GTE ->
            (>=)

        O_EQ ->
            (==)

        O_NEQ ->
            (/=)


largestRegisterValue : Registry -> Maybe Int
largestRegisterValue =
    .registers >> Dict.values >> List.maximum

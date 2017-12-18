module Day18 exposing (..)

import Day18Input exposing (input)
import Dict exposing (Dict)
import List.Extra


output : () -> ( String, String )
output () =
    ( input |> parse |> firstRecovered |> toString
    , ""
    )


type Instruction
    = Isnd String
    | Iset String Value
    | Iadd String Value
    | Imul String Value
    | Imod String Value
    | Ircv String
    | Ijgz String Value


type Value
    = Register String
    | Number Int


type RuntimeError
    = ReferenceError String Instruction
    | ZeroDivisionError Instruction


type Tone
    = Silent
    | Playing Int
    | Recovering Int


type alias Evaluation =
    { registers : Dict String Int
    , tone : Tone
    }


parse : String -> List Instruction
parse =
    String.lines
        >> List.filterMap (parseLine >> Result.toMaybe)


parseLine : String -> Result String Instruction
parseLine string =
    case String.words string of
        [ "snd", register ] ->
            Ok <| Isnd register

        [ "set", register, value ] ->
            Ok <| Iset register (parseValue value)

        [ "add", register, value ] ->
            Ok <| Iadd register (parseValue value)

        [ "mul", register, value ] ->
            Ok <| Imul register (parseValue value)

        [ "mod", register, value ] ->
            Ok <| Imod register (parseValue value)

        [ "rcv", register ] ->
            Ok <| Ircv register

        [ "jgz", register, value ] ->
            Ok <| Ijgz register (parseValue value)

        _ ->
            Err <| "Unknown/invalid operation: " ++ string


parseValue : String -> Value
parseValue string =
    case String.toInt string of
        Ok number ->
            Number number

        Err _ ->
            Register string


emptyEvaluation : Evaluation
emptyEvaluation =
    { registers = Dict.empty, tone = Silent }


evaluate : Instruction -> Evaluation -> Result RuntimeError ( Evaluation, Int )
evaluate instruction ({ registers, tone } as evaluation) =
    let
        getRegister register =
            case Dict.get register registers of
                Just number ->
                    Ok number

                Nothing ->
                    Err <| ReferenceError register instruction

        getValue value =
            case value of
                Register register ->
                    getRegister register

                Number number ->
                    Ok number

        update f register value =
            Result.map2
                (\a b ->
                    f a b
                        |> Result.map
                            (\number ->
                                { evaluation
                                    | registers =
                                        Dict.insert register number registers
                                }
                            )
                )
                (getRegister register)
                (getValue value)
                |> Result.andThen identity

        ok f a b =
            f a b |> Ok

        safeModulo instruction a b =
            if b == 0 then
                Err <| ZeroDivisionError instruction

            else
                Ok <| a % b

        jumpOne =
            Result.map (\result -> ( result, 1 ))
    in
    case instruction of
        Isnd register ->
            getRegister register
                |> Result.map
                    (\number ->
                        { evaluation | tone = Playing number }
                    )
                |> jumpOne

        Iset register value ->
            getValue value
                |> Result.map
                    (\number ->
                        { evaluation
                            | registers = Dict.insert register number registers
                        }
                    )
                |> jumpOne

        Iadd register value ->
            update (ok (+)) register value
                |> jumpOne

        Imul register value ->
            update (ok (*)) register value
                |> jumpOne

        Imod register value ->
            update (safeModulo instruction) register value
                |> jumpOne

        Ircv register ->
            getRegister register
                |> Result.map
                    (\number ->
                        if number == 0 then
                            evaluation

                        else
                            case tone of
                                Silent ->
                                    evaluation

                                Playing playingNumber ->
                                    { evaluation
                                        | tone = Recovering playingNumber
                                    }

                                Recovering _ ->
                                    evaluation
                    )
                |> jumpOne

        Ijgz register value ->
            Result.map2
                (\registerValue jumpValue ->
                    if registerValue > 0 then
                        ( evaluation, jumpValue )

                    else
                        ( evaluation, 1 )
                )
                (getRegister register)
                (getValue value)


firstRecovered : List Instruction -> Result RuntimeError (Maybe Int)
firstRecovered instructions =
    firstRecoveredHelper instructions 0 emptyEvaluation


firstRecoveredHelper :
    List Instruction
    -> Int
    -> Evaluation
    -> Result RuntimeError (Maybe Int)
firstRecoveredHelper instructions pos evaluation =
    case evaluation.tone of
        Recovering number ->
            Ok (Just number)

        _ ->
            case List.Extra.getAt pos instructions of
                Just instruction ->
                    evaluate instruction evaluation
                        |> Result.andThen
                            (\( newEvaluation, jump ) ->
                                firstRecoveredHelper
                                    instructions
                                    (pos + jump)
                                    newEvaluation
                            )

                Nothing ->
                    Ok Nothing

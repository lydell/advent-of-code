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


evaluate : Instruction -> Evaluation -> ( Evaluation, Int )
evaluate instruction ({ registers, tone } as evaluation) =
    let
        getRegister register =
            Dict.get register registers

        getValue value =
            case value of
                Register register ->
                    getRegister register

                Number number ->
                    Just number

        update f register value =
            Maybe.map2
                (\a b ->
                    { evaluation
                        | registers =
                            Dict.insert register (f a b) registers
                    }
                )
                (getRegister register)
                (getValue value)
                |> Maybe.withDefault evaluation

        safeModulo a b =
            if b == 0 then
                a

            else
                a % b

        jumpOne result =
            ( result, 1 )
    in
    case instruction of
        Isnd register ->
            getRegister register
                |> Maybe.map
                    (\number ->
                        { evaluation | tone = Playing number }
                    )
                |> Maybe.withDefault evaluation
                |> jumpOne

        Iset register value ->
            getValue value
                |> Maybe.map
                    (\number ->
                        { evaluation
                            | registers = Dict.insert register number registers
                        }
                    )
                |> Maybe.withDefault evaluation
                |> jumpOne

        Iadd register value ->
            update (+) register value
                |> jumpOne

        Imul register value ->
            update (*) register value
                |> jumpOne

        Imod register value ->
            update safeModulo register value
                |> jumpOne

        Ircv register ->
            getRegister register
                |> Maybe.map
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
                |> Maybe.withDefault evaluation
                |> jumpOne

        Ijgz register value ->
            Maybe.map2
                (\registerValue jumpValue ->
                    if registerValue > 0 then
                        ( evaluation, jumpValue )

                    else
                        ( evaluation, 1 )
                )
                (getRegister register)
                (getValue value)
                |> Maybe.withDefault (jumpOne evaluation)


evaluateFromList :
    List Instruction
    -> ( Evaluation, Int )
    -> Maybe ( Evaluation, Int )
evaluateFromList instructions ( evaluation, pos ) =
    List.Extra.getAt pos instructions
        |> Maybe.map (flip evaluate evaluation >> Tuple.mapSecond ((+) pos))


evaluateN : Int -> List Instruction -> Maybe ( Evaluation, Int )
evaluateN n instructions =
    List.foldl
        (always (Maybe.andThen (evaluateFromList instructions)))
        (Just ( emptyEvaluation, 0 ))
        (List.repeat n ())


firstRecovered : List Instruction -> Maybe Int
firstRecovered instructions =
    firstRecoveredHelper instructions ( emptyEvaluation, 0 )


firstRecoveredHelper :
    List Instruction
    -> ( Evaluation, Int )
    -> Maybe Int
firstRecoveredHelper instructions ( evaluation, pos ) =
    case evaluation.tone of
        Recovering number ->
            Just number

        _ ->
            evaluateFromList instructions ( evaluation, pos )
                |> Maybe.andThen (firstRecoveredHelper instructions)

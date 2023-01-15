module Day18 exposing (..)

import Day18Input exposing (input)
import Dict exposing (Dict)
import List.Extra


output : () -> ( String, String )
output () =
    ( input |> parse |> firstReceived |> toString
    , input |> parse |> duet |> Tuple.second |> .numSent |> toString
    )


type Instruction
    = Isnd Value
    | Iset String Value
    | Iadd String Value
    | Imul String Value
    | Imod String Value
    | Ircv String
    | Ijgz Value Value


type Value
    = Register String
    | Number Int


type alias Registers =
    Dict String Int


type alias Program =
    { registers : Registers
    , queue : List Int
    , pos : Int
    , numSent : Int
    }


type EvaluationResult
    = Next Registers (List Int)
    | Jump Int
    | Send Int
    | Receive (Maybe Int)
    | Ignore


type ProgramResult
    = Updated Program
    | Sending Int Program
    | Receiving (Maybe Int)
    | OutOfRange


emptyProgram : Program
emptyProgram =
    { registers = Dict.empty
    , queue = []
    , pos = 0
    , numSent = 0
    }


parse : String -> List Instruction
parse =
    String.lines
        >> List.filterMap (parseLine >> Result.toMaybe)


parseLine : String -> Result String Instruction
parseLine string =
    case String.words string of
        [ "snd", value ] ->
            Ok <| Isnd (parseValue value)

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

        [ "jgz", value1, value2 ] ->
            Ok <| Ijgz (parseValue value1) (parseValue value2)

        _ ->
            Err <| "Unknown/invalid operation: " ++ string


parseValue : String -> Value
parseValue string =
    case String.toInt string of
        Ok number ->
            Number number

        Err _ ->
            Register string


safeModulo : Int -> Int -> Int
safeModulo a b =
    if b == 0 then
        a

    else
        a % b


evaluate : Instruction -> Registers -> List Int -> EvaluationResult
evaluate instruction registers queue =
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
            case ( getRegister register, getValue value ) of
                ( Just a, Just b ) ->
                    let
                        newRegisters =
                            Dict.insert register (f a b) registers
                    in
                    Next newRegisters queue

                _ ->
                    Ignore
    in
    case instruction of
        Isnd value ->
            case getValue value of
                Just number ->
                    Send number

                Nothing ->
                    Ignore

        Iset register value ->
            case getValue value of
                Just number ->
                    let
                        newRegisters =
                            Dict.insert register number registers
                    in
                    Next newRegisters queue

                Nothing ->
                    Ignore

        Iadd register value ->
            update (+) register value

        Imul register value ->
            update (*) register value

        Imod register value ->
            update safeModulo register value

        Ircv register ->
            case queue of
                [] ->
                    Receive (getRegister register)

                number :: restQueue ->
                    let
                        newRegisters =
                            Dict.insert register number registers
                    in
                    Next newRegisters restQueue

        Ijgz value1 value2 ->
            case ( getValue value1, getValue value2 ) of
                ( Just registerValue, Just jumpValue ) ->
                    if registerValue > 0 then
                        Jump jumpValue

                    else
                        Ignore

                _ ->
                    Ignore


updateProgram : List Instruction -> Program -> ProgramResult
updateProgram instructions program =
    case List.Extra.getAt program.pos instructions of
        Just instruction ->
            case evaluate instruction program.registers program.queue of
                Next newRegisters newQueue ->
                    Updated
                        { program
                            | registers = newRegisters
                            , queue = newQueue
                            , pos = program.pos + 1
                        }

                Jump jump ->
                    Updated { program | pos = program.pos + jump }

                Send number ->
                    Sending number
                        { program
                            | numSent = program.numSent + 1
                            , pos = program.pos + 1
                        }

                Receive currentValue ->
                    Receiving currentValue

                Ignore ->
                    Updated { program | pos = program.pos + 1 }

        Nothing ->
            OutOfRange


firstReceived : List Instruction -> Maybe Int
firstReceived instructions =
    firstReceivedHelper instructions emptyProgram Nothing


firstReceivedHelper : List Instruction -> Program -> Maybe Int -> Maybe Int
firstReceivedHelper instructions program lastSentNumber =
    case updateProgram instructions program of
        Updated newProgram ->
            firstReceivedHelper instructions newProgram lastSentNumber

        Sending number newProgram ->
            firstReceivedHelper instructions newProgram (Just number)

        Receiving currentValue ->
            let
                newProgram =
                    { program | pos = program.pos + 1 }
            in
            case currentValue of
                Just 0 ->
                    firstReceivedHelper instructions newProgram lastSentNumber

                Just _ ->
                    lastSentNumber

                Nothing ->
                    firstReceivedHelper instructions newProgram lastSentNumber

        OutOfRange ->
            Nothing


updateDuetProgram : List Instruction -> Program -> ( Program, Maybe Int, Bool )
updateDuetProgram instructions program =
    case updateProgram instructions program of
        Updated newProgram ->
            ( newProgram, Nothing, True )

        Sending number newProgram ->
            ( newProgram, Just number, True )

        Receiving _ ->
            ( program, Nothing, False )

        OutOfRange ->
            ( program, Nothing, False )


duet : List Instruction -> ( Program, Program )
duet instructions =
    let
        program0 =
            { emptyProgram | registers = Dict.singleton "p" 0 }

        program1 =
            { emptyProgram | registers = Dict.singleton "p" 1 }
    in
    duetHelper instructions ( program0, program1 )


duetHelper : List Instruction -> ( Program, Program ) -> ( Program, Program )
duetHelper instructions ( program0, program1 ) =
    let
        addToQueue maybeNumber program =
            case maybeNumber of
                Just number ->
                    { program | queue = program.queue ++ [ number ] }

                Nothing ->
                    program

        ( newProgram0, sent0, running0 ) =
            updateDuetProgram instructions program0

        ( newProgram1, sent1, running1 ) =
            updateDuetProgram instructions program1

        finalProgram0 =
            addToQueue sent1 newProgram0

        finalProgram1 =
            addToQueue sent0 newProgram1
    in
    if not running0 && not running1 then
        ( finalProgram0, finalProgram1 )

    else
        -- ( finalProgram0, finalProgram1 )
        duetHelper instructions ( finalProgram0, finalProgram1 )

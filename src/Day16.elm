module Day16 exposing (..)

import Day16Input exposing (input)
import List.Extra


output : () -> ( String, String )
output () =
    ( evaluate (parse input) dancers16 |> String.join ""
    , ""
    )


type Instruction
    = Spin Int
    | Exchange Int Int
    | Partner String String


parse : String -> List Instruction
parse =
    String.split ","
        >> List.filterMap (parseInstruction >> Result.toMaybe)


parseInstruction : String -> Result String Instruction
parseInstruction string =
    case String.uncons string of
        Just ( 's', rest ) ->
            String.toInt rest |> Result.map Spin

        Just ( 'x', rest ) ->
            case String.split "/" rest |> List.map String.toInt of
                [ Ok numberA, Ok numberB ] ->
                    Ok <| Exchange numberA numberB

                _ ->
                    Err <| "Expected two numbers separated by a slash in: " ++ string

        Just ( 'p', rest ) ->
            case String.split "/" rest |> List.map String.uncons of
                [ Just ( a, aRest ), Just ( b, bRest ) ] ->
                    Ok <| Partner (String.cons a aRest) (String.cons b bRest)

                _ ->
                    Err <| "Expected two non-empty names separated by a slash in: " ++ string

        Just ( char, _ ) ->
            Err <| "Unknown instruction '" ++ String.fromChar char ++ "' in: " ++ string

        Nothing ->
            Err "Unexpected empty string"


evaluate : List Instruction -> List String -> List String
evaluate instructions names =
    case instructions of
        [] ->
            names

        instruction :: restInstructions ->
            evaluate restInstructions (evaluateInstruction instruction names)


evaluateInstruction : Instruction -> List String -> List String
evaluateInstruction instruction names =
    case instruction of
        Spin number ->
            let
                ( before, after ) =
                    List.Extra.splitAt (List.length names - number) names
            in
            after ++ before

        Exchange aIndex bIndex ->
            List.Extra.swapAt aIndex bIndex names
                |> Maybe.withDefault names

        Partner aName bName ->
            case ( List.Extra.elemIndex aName names, List.Extra.elemIndex bName names ) of
                ( Just aIndex, Just bIndex ) ->
                    List.Extra.swapAt aIndex bIndex names
                        |> Maybe.withDefault names

                _ ->
                    names


dancers16 : List String
dancers16 =
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p" ]

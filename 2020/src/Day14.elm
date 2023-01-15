module Day14 exposing (..)

import Binary
import Day14Input exposing (puzzleInput)
import Dict exposing (Dict)
import Html exposing (Html)
import LineParser


type Instruction
    = Mask (List MaskBit)
    | Mem Int Int


type MaskBit
    = Zero
    | One
    | X


parse : String -> Result String (List Instruction)
parse =
    LineParser.parse
        (\line ->
            case String.split " = " line of
                [ left, right ] ->
                    if left == "mask" then
                        String.toList right
                            |> LineParser.parseGeneral "MaskBit"
                                String.fromChar
                                (\char ->
                                    case char of
                                        '0' ->
                                            Ok Zero

                                        '1' ->
                                            Ok One

                                        'X' ->
                                            Ok X

                                        _ ->
                                            Err "Unknown MaskBit."
                                )
                            |> Result.map Mask

                    else if (left |> String.startsWith "mem[") && (left |> String.endsWith "]") then
                        Result.map2 Mem
                            (left
                                |> String.dropLeft 4
                                |> String.dropRight 1
                                |> String.toInt
                                |> Result.fromMaybe "Address is not a number."
                            )
                            (right
                                |> String.toInt
                                |> Result.fromMaybe "Value is not a number."
                            )

                    else
                        Err "Expected `mask` or `mem[digits]`"

                _ ->
                    Err "Expected `left = right`."
        )


solution1 : String -> Result String Int
solution1 =
    parse
        >> Result.andThen
            (solve
                (\mask address value ->
                    Dict.insert address (applyMask1 mask value)
                )
            )


applyMask1 : List MaskBit -> Int -> Int
applyMask1 mask value =
    List.map2
        (\maskBit bit ->
            case maskBit of
                Zero ->
                    False

                One ->
                    True

                X ->
                    bit
        )
        mask
        (value
            |> Binary.fromDecimal
            |> Binary.ensureSize (List.length mask)
            |> Binary.toBooleans
        )
        |> Binary.fromBooleans
        |> Binary.toDecimal


solve :
    (List MaskBit -> Int -> Int -> Dict Int Int -> Dict Int Int)
    -> List Instruction
    -> Result String Int
solve updateMemory instructions =
    case instructions of
        [] ->
            Err "Empty list of instructions."

        (Mask firstMask) :: rest ->
            rest
                |> List.foldl
                    (\instruction ( mask, memory ) ->
                        case instruction of
                            Mask nextMask ->
                                ( nextMask, memory )

                            Mem address value ->
                                ( mask, updateMemory mask address value memory )
                    )
                    ( firstMask, Dict.empty )
                |> Tuple.second
                |> Dict.values
                |> List.sum
                |> Ok

        _ ->
            Err "First instruction must be `mask =`."


solution2 : String -> Result String Int
solution2 =
    parse
        >> Result.andThen
            (solve
                (\mask address value memory ->
                    applyMask2 mask address
                        |> List.foldl
                            (\nextAddress ->
                                Dict.insert nextAddress value
                            )
                            memory
                )
            )


applyMask2 : List MaskBit -> Int -> List Int
applyMask2 mask address =
    let
        numX =
            mask
                |> List.filter ((==) X)
                |> List.length

        addressBits =
            address
                |> Binary.fromDecimal
                |> Binary.ensureSize (List.length mask)
                |> Binary.toBooleans
    in
    List.range 0 (2 ^ numX)
        |> List.map
            (\index ->
                let
                    floatingBits =
                        index
                            |> Binary.fromDecimal
                            |> Binary.ensureSize numX
                            |> Binary.toBooleans
                in
                List.map2 Tuple.pair
                    mask
                    addressBits
                    |> List.foldl
                        (\( maskBit, addressBit ) ( remainingFloatingBits, result ) ->
                            case maskBit of
                                Zero ->
                                    ( remainingFloatingBits, addressBit :: result )

                                One ->
                                    ( remainingFloatingBits, True :: result )

                                X ->
                                    case remainingFloatingBits of
                                        [] ->
                                            ( [], False :: result )

                                        first :: rest ->
                                            ( rest, first :: result )
                        )
                        ( floatingBits, [] )
                    |> Tuple.second
                    |> List.reverse
                    |> Binary.fromBooleans
                    |> Binary.toDecimal
            )


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

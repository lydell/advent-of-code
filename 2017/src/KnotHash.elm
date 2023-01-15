module KnotHash exposing (KnotHash, KnotHashInspection, check, dense, fromList, hash256, inspect, next, stepThrough, stepThroughN, stringToCharCodes, toHex)

import Array exposing (Array)
import Bitwise
import Char
import Hex
import List.Extra


type KnotHash
    = KnotHash
        { array : Array Int
        , pos : Int
        , skip : Int
        }


type alias KnotHashInspection =
    { list : List Int
    , pos : Int
    , skip : Int
    }


fromList : List Int -> KnotHash
fromList list =
    KnotHash
        { array = Array.fromList list
        , pos = 0
        , skip = 0
        }


inspect : KnotHash -> KnotHashInspection
inspect (KnotHash { array, pos, skip }) =
    { list = Array.toList array
    , pos = pos
    , skip = skip
    }


next : Int -> KnotHash -> KnotHash
next length (KnotHash { array, pos, skip }) =
    let
        arrayLength =
            Array.length array

        newArray =
            array
                |> Array.indexedMap
                    (\index number ->
                        let
                            localIndex =
                                (index - pos) % arrayLength

                            reverseIndex =
                                length - localIndex - 1

                            finalIndex =
                                (pos + reverseIndex) % arrayLength
                        in
                        if localIndex < length then
                            Array.get finalIndex array
                                |> Maybe.withDefault number
                        else
                            number
                    )
    in
    KnotHash
        { array = newArray
        , pos = (pos + length + skip) % arrayLength
        , skip = skip + 1
        }


stepThrough : List Int -> KnotHash -> KnotHash
stepThrough lengths knotHash =
    List.foldl next knotHash lengths


stepThroughN : Int -> List Int -> KnotHash -> KnotHash
stepThroughN n lengths knotHash =
    List.repeat n ()
        |> List.foldl (always (stepThrough lengths)) knotHash


dense : Int -> KnotHash -> List Int
dense n (KnotHash { array }) =
    array
        |> Array.toList
        |> List.Extra.groupsOf n
        |> List.map (List.foldl Bitwise.xor 0)


toHex : List Int -> String
toHex =
    List.map (Hex.toString >> String.padLeft 2 '0')
        >> String.join ""


stringToCharCodes : String -> List Int
stringToCharCodes string =
    string
        |> String.toList
        |> List.map Char.toCode


standardSuffix : List number
standardSuffix =
    [ 17, 31, 73, 47, 23 ]


hash256 : String -> String
hash256 string =
    let
        lengths =
            stringToCharCodes string ++ standardSuffix

        start =
            fromList (List.range 0 255)
    in
    stepThroughN 64 lengths start
        |> dense 16
        |> toHex


check : KnotHash -> Maybe Int
check (KnotHash { array }) =
    case Array.toList array of
        first :: second :: rest ->
            Just (first * second)

        _ ->
            Nothing

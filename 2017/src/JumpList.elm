module JumpList exposing (JumpList, JumpListInspection, fromList, inspect, iterate, next, withTransform)

import Array exposing (Array)


type JumpList
    = JumpList
        { array : Array Int
        , index : Int
        , numJumps : Int
        , transform : Int -> Int
        }


type alias JumpListInspection =
    { list : List Int
    , index : Int
    , numJumps : Int
    }


fromList : List Int -> JumpList
fromList list =
    JumpList
        { array = Array.fromList list
        , index = 0
        , numJumps = 0
        , transform = (+) 1
        }


withTransform : (Int -> Int) -> JumpList -> JumpList
withTransform transform (JumpList details) =
    JumpList { details | transform = transform }


inspect : JumpList -> JumpListInspection
inspect (JumpList { array, index, numJumps }) =
    { list = Array.toList array
    , index = index
    , numJumps = numJumps
    }


next : JumpList -> JumpList
next jumpList =
    let
        (JumpList { array, index, numJumps, transform }) =
            jumpList
    in
    case Array.get index array of
        Just number ->
            JumpList
                { array = Array.set index (transform number) array
                , index = index + number
                , numJumps = numJumps + 1
                , transform = transform
                }

        Nothing ->
            jumpList


iterate : JumpList -> JumpList
iterate jumpList =
    let
        (JumpList { index, array }) =
            jumpList
    in
    if index < 0 || index >= Array.length array then
        jumpList
    else
        iterate (next jumpList)

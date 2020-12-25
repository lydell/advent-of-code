module Day25 exposing (..)

import Day25Input exposing (puzzleInput)
import Html exposing (Html)


solution1 : Int -> ( Int, Int ) -> Result String Int
solution1 s ( first, second ) =
    { --first = ( first, findLoopSize 7 first )
      -- , second = ( second, findSubjectNumberAndLoopSize second )
      --   first = ( first, transformAtMost 100000000 7 first ( 1, 0 ) )
      -- , second = ( second, transformAtMost 100000000 7 second ( 1, 0 ) )
      enc = transformAtMost s first -1 ( 1, 0 )

    -- , tmash2 = List.repeat 11 () |> List.foldl (always (transform 7)) 1
    -- , tmash3 = List.range 1 20 |> List.map (\n -> List.repeat n () |> List.foldl (always (transform 7)) 1)
    }
        |> Debug.toString
        |> Err


findSubjectNumberAndLoopSize : Int -> ( Int, Int )
findSubjectNumberAndLoopSize target =
    findSubjectNumberAndLoopSizeHelper target 2


findSubjectNumberAndLoopSizeHelper : Int -> Int -> ( Int, Int )
findSubjectNumberAndLoopSizeHelper target currentSubjectNumber =
    let
        ( final, count ) =
            transformAtMost 5 currentSubjectNumber target ( 1, 0 )
    in
    if final == target then
        ( currentSubjectNumber, count )

    else
        findSubjectNumberAndLoopSizeHelper target (currentSubjectNumber + 1)


transformAtMost : Int -> Int -> Int -> ( Int, Int ) -> ( Int, Int )
transformAtMost maxCount subjectNumber target ( n, count ) =
    if n == target then
        ( n, count )

    else if count >= maxCount then
        ( n, count )

    else
        transformAtMost maxCount subjectNumber target ( transform subjectNumber n, count + 1 )


transform : Int -> Int -> Int
transform subjectNumber n =
    n * subjectNumber |> remainderBy 20201227


main : Html Never
main =
    Html.div []
        [ showResult (solution1 11 exampleInput)
        , showResult (solution1 8516638 puzzleInput)
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


exampleInput : ( Int, Int )
exampleInput =
    -- (card, door)
    ( 5764801, 17807724 )

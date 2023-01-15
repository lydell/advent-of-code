module Day25 exposing (..)

import Html exposing (Html)


type LoopSize
    = First Int
    | Second Int


solution : ( Int, Int ) -> Int
solution ( first, second ) =
    let
        ( subjectNumber, loopSize ) =
            case findLoopSize ( first, second ) ( 1, 0 ) of
                First count ->
                    ( second, count )

                Second count ->
                    ( first, count )
    in
    List.repeat loopSize ()
        |> List.foldl
            (always (transform subjectNumber))
            1


findLoopSize : ( Int, Int ) -> ( Int, Int ) -> LoopSize
findLoopSize ( first, second ) ( n, count ) =
    if first == n then
        First count

    else if second == n then
        Second count

    else
        findLoopSize ( first, second ) ( transform 7 n, count + 1 )


transform : Int -> Int -> Int
transform subjectNumber n =
    n * subjectNumber |> remainderBy 20201227


main : Html Never
main =
    Html.div []
        [ showResult (solution exampleInput)
        , showResult (solution puzzleInput)
        ]


showResult : Int -> Html msg
showResult result =
    Html.output []
        [ Html.text (String.fromInt result)
        ]


exampleInput : ( Int, Int )
exampleInput =
    ( 5764801, 17807724 )


puzzleInput : ( Int, Int )
puzzleInput =
    ( 11404017, 13768789 )

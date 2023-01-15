module Day3 exposing (..)

import Day3Input exposing (puzzleInput)
import Html exposing (Html)
import Set exposing (Set)


parse : String -> ( Int, List (Set Int) )
parse input =
    let
        lines =
            input
                |> String.trim
                |> String.lines
    in
    ( lines |> List.head |> Maybe.map String.length |> Maybe.withDefault 0
    , lines |> List.map (String.indexes "#" >> Set.fromList)
    )


solution1 : String -> Int
solution1 input =
    let
        ( width, parsed ) =
            parse input
    in
    countTrees { right = 3, down = 1, width = width } parsed


countTrees : { right : Int, down : Int, width : Int } -> List (Set Int) -> Int
countTrees { right, down, width } =
    List.drop down
        >> List.indexedMap
            (\index trees ->
                if (index |> modBy down) == 0 then
                    let
                        pos =
                            (right * (index // down + 1))
                                |> modBy width
                    in
                    Set.member pos trees

                else
                    False
            )
        >> List.filter identity
        >> List.length


solution2 : String -> Int
solution2 input =
    let
        ( width, parsed ) =
            parse input
    in
    [ ( 1, 1 )
    , ( 3, 1 )
    , ( 5, 1 )
    , ( 7, 1 )
    , ( 1, 2 )
    ]
        |> List.map
            (\( right, down ) ->
                countTrees
                    { right = right, down = down, width = width }
                    parsed
            )
        |> List.product


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
        , showResult (solution2 puzzleInput)
        ]


showResult : Int -> Html msg
showResult result =
    Html.output []
        [ Html.text (String.fromInt result) ]

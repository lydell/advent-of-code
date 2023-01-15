module Day11 exposing (..)

import Array exposing (Array)
import Day11Input exposing (puzzleInput)
import Html exposing (Html)
import LineParser
import Matrix exposing (Matrix)
import Matrix.Extra
import Neighbours
import Set


type Spot
    = Floor
    | EmptySeat
    | OccupiedSeat


parse : String -> Result String (Matrix Spot)
parse =
    LineParser.parse
        (String.toList
            >> LineParser.parseGeneral "Spot" String.fromChar parseSpot
        )
        >> Result.andThen (Matrix.Extra.toMatrix Floor)


parseSpot : Char -> Result String Spot
parseSpot char =
    case char of
        '.' ->
            Ok Floor

        'L' ->
            Ok EmptySeat

        '#' ->
            Ok OccupiedSeat

        _ ->
            Err "Unknown Spot."


solution1 : String -> Result String Int
solution1 =
    parse >> Result.map (solve (step 4 getOccupied1))


getOccupied1 : Int -> Int -> Matrix Spot -> Int
getOccupied1 x y matrix =
    Neighbours.neighbours Neighbours.Plane x y matrix
        |> Array.filter ((==) OccupiedSeat)
        |> Array.length


solution2 : String -> Result String Int
solution2 =
    parse >> Result.map (solve (step 5 getOccupied2))


getOccupied2 : Int -> Int -> Matrix Spot -> Int
getOccupied2 x y matrix =
    [ -- Up
      ( 0, -1 )
    , -- Right-Up
      ( 1, -1 )
    , -- Right
      ( 1, 0 )
    , -- Right-Down
      ( 1, 1 )
    , -- Down
      ( 0, 1 )
    , -- Left-Down
      ( -1, 1 )
    , -- Left
      ( -1, 0 )
    , -- Left-Up
      ( -1, -1 )
    ]
        |> List.filter (getOccupied2Helper matrix ( x, y ))
        |> List.length


getOccupied2Helper : Matrix Spot -> ( Int, Int ) -> ( Int, Int ) -> Bool
getOccupied2Helper matrix ( x, y ) ( dx, dy ) =
    let
        ( nextX, nextY ) =
            ( x + dx, y + dy )
    in
    case Matrix.get nextX nextY matrix of
        Ok Floor ->
            getOccupied2Helper matrix ( nextX, nextY ) ( dx, dy )

        Ok EmptySeat ->
            False

        Ok OccupiedSeat ->
            True

        Err _ ->
            False


solve : (Matrix Spot -> Matrix Spot) -> Matrix Spot -> Int
solve step_ matrix =
    let
        nextMatrix =
            step_ matrix
    in
    if nextMatrix == matrix then
        nextMatrix
            |> Matrix.toArray
            |> Array.filter ((==) OccupiedSeat)
            |> Array.length

    else
        solve step_ nextMatrix


step : Int -> (Int -> Int -> Matrix Spot -> Int) -> Matrix Spot -> Matrix Spot
step limit getOccupied matrix =
    matrix
        |> Matrix.indexedMap
            (\x y spot ->
                case spot of
                    Floor ->
                        Floor

                    EmptySeat ->
                        if getOccupied x y matrix == 0 then
                            OccupiedSeat

                        else
                            EmptySeat

                    OccupiedSeat ->
                        if getOccupied x y matrix >= limit then
                            EmptySeat

                        else
                            OccupiedSeat
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

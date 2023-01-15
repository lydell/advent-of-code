module Day17 exposing (..)

import Day17Input exposing (puzzleInput)
import Html exposing (Html)
import LineParser
import Set exposing (Set)


type alias ActiveCoords =
    Set ( Int, Int, Int )


type State
    = Active
    | Inactive


parse : String -> Result String ActiveCoords
parse =
    LineParser.parse
        (String.toList
            >> LineParser.parseGeneral "Item"
                String.fromChar
                (\char ->
                    case char of
                        '#' ->
                            Ok Active

                        '.' ->
                            Ok Inactive

                        _ ->
                            Err "Unknown item."
                )
        )
        >> Result.map
            (List.indexedMap
                (\y ->
                    List.indexedMap
                        (\x active ->
                            case active of
                                Active ->
                                    Just ( x, y, 0 )

                                Inactive ->
                                    Nothing
                        )
                )
                >> List.concat
                >> List.filterMap identity
                >> Set.fromList
            )


solution : String -> Result String Int
solution =
    parse >> Result.map (cycle 6 >> Set.size)


cycle : Int -> ActiveCoords -> ActiveCoords
cycle cyclesLeft activeCoords =
    if cyclesLeft <= 0 then
        activeCoords

    else
        activeCoords
            |> getBounds
            |> expandBounds 1
            |> boundsToCoords
            |> List.filterMap
                (\coord ->
                    let
                        numActiveNeighbors =
                            getNeighbors coord activeCoords
                                |> List.filter ((==) Active)
                                |> List.length

                        state =
                            if Set.member coord activeCoords then
                                Active

                            else
                                Inactive
                    in
                    case state of
                        Active ->
                            if numActiveNeighbors == 2 || numActiveNeighbors == 3 then
                                Just coord

                            else
                                Nothing

                        Inactive ->
                            if numActiveNeighbors == 3 then
                                Just coord

                            else
                                Nothing
                )
            |> Set.fromList
            |> cycle (cyclesLeft - 1)


type alias Bounds =
    ( ( Int, Int ), ( Int, Int ), ( Int, Int ) )


getBounds : ActiveCoords -> Bounds
getBounds activeCoords =
    let
        list =
            Set.toList activeCoords

        xs =
            list |> List.map (\( x, _, _ ) -> x)

        ys =
            list |> List.map (\( _, y, _ ) -> y)

        zs =
            list |> List.map (\( _, _, z ) -> z)
    in
    ( ( xs |> List.minimum |> Maybe.withDefault 0
      , xs |> List.maximum |> Maybe.withDefault 0
      )
    , ( ys |> List.minimum |> Maybe.withDefault 0
      , ys |> List.maximum |> Maybe.withDefault 0
      )
    , ( zs |> List.minimum |> Maybe.withDefault 0
      , zs |> List.maximum |> Maybe.withDefault 0
      )
    )


expandBounds : Int -> Bounds -> Bounds
expandBounds size ( ( minX, maxX ), ( minY, maxY ), ( minZ, maxZ ) ) =
    ( ( minX - size, maxX + size )
    , ( minY - size, maxY + size )
    , ( minZ - size, maxZ + size )
    )


boundsToCoords : Bounds -> List ( Int, Int, Int )
boundsToCoords ( ( minX, maxX ), ( minY, maxY ), ( minZ, maxZ ) ) =
    List.range minX maxX
        |> List.concatMap
            (\x ->
                List.range minY maxY
                    |> List.concatMap
                        (\y ->
                            List.range minZ maxZ
                                |> List.map
                                    (\z ->
                                        ( x, y, z )
                                    )
                        )
            )


neighborOffsets : List ( Int, Int, Int )
neighborOffsets =
    [ ( 0, 0, -1 )
    , ( 0, 0, 1 )
    , ( 0, -1, 0 )
    , ( 0, -1, -1 )
    , ( 0, -1, 1 )
    , ( 0, 1, 0 )
    , ( 0, 1, -1 )
    , ( 0, 1, 1 )
    , ( -1, 0, 0 )
    , ( -1, 0, -1 )
    , ( -1, 0, 1 )
    , ( -1, -1, 0 )
    , ( -1, -1, -1 )
    , ( -1, -1, 1 )
    , ( -1, 1, 0 )
    , ( -1, 1, -1 )
    , ( -1, 1, 1 )
    , ( 1, 0, 0 )
    , ( 1, 0, -1 )
    , ( 1, 0, 1 )
    , ( 1, -1, 0 )
    , ( 1, -1, -1 )
    , ( 1, -1, 1 )
    , ( 1, 1, 0 )
    , ( 1, 1, -1 )
    , ( 1, 1, 1 )
    ]


getNeighbors : ( Int, Int, Int ) -> ActiveCoords -> List State
getNeighbors ( x, y, z ) activeCoords =
    neighborOffsets
        |> List.map
            (\( dx, dy, dz ) ->
                if Set.member ( x + dx, y + dy, z + dz ) activeCoords then
                    Active

                else
                    Inactive
            )


main : Html Never
main =
    Html.div []
        [ showResult (solution puzzleInput)
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

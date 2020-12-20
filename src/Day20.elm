module Day20 exposing (..)

import Array exposing (Array)
import Day20Input exposing (puzzleInput)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import LineParser
import Matrix exposing (Matrix)
import Matrix.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Result
import Svg.Attributes exposing (rotate)
import TemplateSandbox exposing (Msg)


originalSize : Int
originalSize =
    10


type alias Tile =
    { edges : List Edge
    , image : Matrix Color
    , imageWithBorders : Matrix Color
    }


type alias Edge =
    { turns : Int
    , colors : Array Color
    }


type Color
    = White
    | Black


parse : String -> Result String (List ( Int, Tile ))
parse =
    String.trim
        >> String.split "\n\n"
        >> LineParser.parseGeneral "Tile"
            identity
            (Parser.run tileParser
                >> Result.mapError Parser.Extra.deadEndsToString
            )
        >> Result.andThen
            (LineParser.parseGeneral "Intermediate tile"
                (Tuple.first >> String.fromInt)
                (\( id, matrix ) ->
                    Result.map4
                        (\top bottom left right ->
                            ( id
                            , { edges =
                                    [ { turns = 0
                                      , colors = top
                                      }
                                    , { turns = 1
                                      , colors = right
                                      }
                                    , { turns = 2
                                      , colors = reverseArray bottom
                                      }
                                    , { turns = 3
                                      , colors = reverseArray left
                                      }
                                    ]
                              , image = trimBorder White matrix
                              , imageWithBorders = matrix
                              }
                            )
                        )
                        (Matrix.getRow 0 matrix)
                        (Matrix.getRow (Matrix.width matrix - 1) matrix)
                        (Matrix.getColumn 0 matrix)
                        (Matrix.getColumn (Matrix.height matrix - 1) matrix)
                )
            )


reverseArray : Array a -> Array a
reverseArray =
    Array.foldr Array.push Array.empty


trimBorder : a -> Matrix a -> Matrix a
trimBorder default matrix =
    Matrix.generate
        (Matrix.width matrix - 2)
        (Matrix.height matrix - 2)
        (\x y ->
            Matrix.get (x + 1) (y + 1) matrix
                |> Result.toMaybe
                |> Maybe.withDefault default
        )


tileParser : Parser ( Int, Matrix Color )
tileParser =
    Parser.succeed Tuple.pair
        |. Parser.symbol "Tile"
        |. Parser.Extra.spaces
        |= Parser.int
        |. Parser.Extra.spaces
        |. Parser.symbol ":"
        |. Parser.Extra.spaces
        |. Parser.symbol "\n"
        |= matrixParser


matrixParser : Parser (Matrix Color)
matrixParser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.Extra.spaces
        , item =
            colorParser
                |> Parser.andThen
                    (\firstColor ->
                        Parser.loop [ firstColor ]
                            (\result ->
                                Parser.oneOf
                                    [ Parser.succeed (\color -> Parser.Loop (color :: result))
                                        |= colorParser
                                    , Parser.succeed ()
                                        |> Parser.map (\() -> Parser.Done (List.reverse result))
                                    ]
                            )
                    )
        , trailing = Parser.Forbidden
        }
        |> Parser.andThen
            (\listsOfLists ->
                case Matrix.Extra.toMatrix White listsOfLists of
                    Ok matrix ->
                        if Matrix.width matrix /= originalSize || Matrix.width matrix /= originalSize then
                            Parser.problem
                                ("Expected a "
                                    ++ String.fromInt originalSize
                                    ++ "x"
                                    ++ String.fromInt originalSize
                                    ++ " matrix but got "
                                    ++ String.fromInt (Matrix.width matrix)
                                    ++ "x"
                                    ++ String.fromInt (Matrix.height matrix)
                                )

                        else
                            Parser.succeed matrix

                    Err message ->
                        Parser.problem message
            )


colorParser : Parser Color
colorParser =
    Parser.oneOf
        [ Parser.succeed White
            |. Parser.symbol "."
        , Parser.succeed Black
            |. Parser.symbol "#"
        ]


puzzle : List ( Int, Tile ) -> Result String ( Int, Matrix Color, Matrix Color )
puzzle tiles =
    case tiles of
        [] ->
            Err "Empty list of tiles."

        ( firstTileId, firstTile ) :: rest ->
            let
                ( tilesLeft, result ) =
                    puzzleHelper
                        ( 0, 0 )
                        firstTileId
                        firstTile
                        ( Dict.fromList rest, Dict.singleton ( 0, 0 ) ( firstTileId, firstTile ) )

                _ =
                    let
                        width =
                            maxX - minX + 1

                        height =
                            maxY - minY + 1

                        mat =
                            result
                                |> Dict.toList
                                |> List.foldl
                                    (\( ( x, y ), ( id, _ ) ) matrix ->
                                        Matrix.set (x - minX) (y - minY) id matrix
                                    )
                                    (Matrix.repeat width height 0)
                    in
                    List.range 0 (Matrix.height mat - 1)
                        |> List.filterMap (\y -> Matrix.getRow y mat |> Result.toMaybe |> Maybe.map (Array.toList >> Debug.toString))
                        |> String.join "  "
                        |> Debug.log "puzzle"

                coords =
                    Dict.keys result

                xs =
                    List.map Tuple.first coords

                ys =
                    List.map Tuple.second coords

                minX =
                    List.minimum xs |> Maybe.withDefault 0

                maxX =
                    List.maximum xs |> Maybe.withDefault 0

                minY =
                    List.minimum ys |> Maybe.withDefault 0

                maxY =
                    List.maximum ys |> Maybe.withDefault 0

                cornerIdProduct =
                    [ ( minX, minY )
                    , ( minX, maxY )
                    , ( maxX, minY )
                    , ( maxX, maxY )
                    ]
                        |> List.filterMap (\coord -> Dict.get coord result |> Maybe.map Tuple.first)
                        |> List.product

                joinImage size imageGetter =
                    let
                        width =
                            (maxX - minX + 1) * size

                        height =
                            (maxY - minY + 1) * size
                    in
                    result
                        |> Dict.toList
                        |> List.foldl
                            (\( ( x, y ), ( _, tile ) ) matrix ->
                                tile
                                    |> imageGetter
                                    |> Matrix.indexedMap (\ix iy color -> ( ix, iy, color ))
                                    |> Matrix.foldl
                                        (\( ix, iy, color ) ->
                                            Matrix.set
                                                ((x - minX) * size + ix)
                                                ((y - minY) * size + iy)
                                                color
                                        )
                                        matrix
                            )
                            (Matrix.repeat width height White)

                -- |> flipMatrixAroundXAxis White
                _ =
                    Debug.log "Left to consume" (tilesLeft |> Dict.toList |> List.map (Tuple.first >> String.fromInt) |> String.join ", ")
            in
            if Dict.size tilesLeft == -1 then
                Err
                    ("All tiles were not consumed: "
                        ++ (tilesLeft |> Dict.toList |> List.map (Tuple.first >> String.fromInt) |> String.join ", ")
                    )

            else
                Ok
                    ( cornerIdProduct
                    , joinImage (originalSize - 2) .image
                    , joinImage (originalSize + 2) .imageWithBorders
                    )


puzzleHelper : ( Int, Int ) -> Int -> Tile -> ( Dict Int Tile, Dict ( Int, Int ) ( Int, Tile ) ) -> ( Dict Int Tile, Dict ( Int, Int ) ( Int, Tile ) )
puzzleHelper ( x, y ) tileId tile ( initialTilesLeft, initialResult ) =
    let
        _ =
            Debug.log "puzzleHelper" tileId

        nextTiles =
            tile.edges
                |> List.filterMap
                    (\edge ->
                        findNextTile edge initialTilesLeft
                            |> Maybe.map (Tuple.pair edge)
                    )
                |> (\list ->
                        let
                            _ =
                                list
                                    |> List.map (\( edge, ( id, _ ) ) -> ( edge.turns, id ))
                                    |> Debug.log "next tiles"
                        in
                        list
                   )

        tilesLeftWithoutNext =
            nextTiles
                |> List.foldl (\( _, ( id, _ ) ) -> Dict.remove id) initialTilesLeft
    in
    nextTiles
        |> List.foldl
            (\( edge, ( nextTileId, nextTile ) ) ( tilesLeft, result ) ->
                let
                    nextCoord =
                        case edge.turns |> modBy 4 of
                            0 ->
                                ( x, y - 1 )

                            1 ->
                                ( x + 1, y )

                            2 ->
                                ( x, y + 1 )

                            3 ->
                                ( x - 1, y )

                            _ ->
                                ( x, y )

                    nextResult =
                        Dict.insert nextCoord ( nextTileId, nextTile ) result

                    _ =
                        Debug.log "fold" ( tileId, nextTileId, ( edge.turns, ( x, y ), nextCoord ) )
                in
                puzzleHelper nextCoord nextTileId nextTile ( tilesLeft, nextResult )
            )
            ( tilesLeftWithoutNext
            , initialResult
            )


findNextTile : Edge -> Dict Int Tile -> Maybe ( Int, Tile )
findNextTile wantedEdge =
    Dict.foldl
        (\id tile result ->
            case result of
                Just x ->
                    Just x

                Nothing ->
                    tile.edges
                        |> List.filterMap
                            (\edge ->
                                if edge.colors == wantedEdge.colors then
                                    Just ( True, edge )

                                else if reverseArray edge.colors == wantedEdge.colors then
                                    Just ( False, edge )

                                else
                                    Nothing
                            )
                        |> List.head
                        |> Maybe.map
                            (\( flip, edge ) ->
                                ( id
                                , transformTile id flip wantedEdge edge tile
                                )
                            )
        )
        Nothing


transformTile : Int -> Bool -> Edge -> Edge -> Tile -> Tile
transformTile id flip otherEdge edge tile =
    let
        _ =
            Debug.log "transformTile"
                ( id
                , ( edge.turns, otherEdge.turns, otherEdge.turns + 2 - edge.turns )
                , if not flip then
                    "identity"

                  else if edge.turns |> modBy 2 |> (==) 0 then
                    "flipTileAroundYAxis"

                  else
                    "flipTileAroundXAxis"
                )
    in
    (if not flip then
        tile

     else if edge.turns |> modBy 2 |> (==) 0 then
        flipTileAroundYAxis tile

     else
        flipTileAroundXAxis tile
    )
        |> rotateTile (otherEdge.turns + 2 - edge.turns)


flipTileAroundXAxis : Tile -> Tile
flipTileAroundXAxis tile =
    { edges =
        tile.edges
            |> List.map
                (\edge ->
                    if edge.turns |> modBy 2 |> (==) 0 then
                        { edge | turns = edge.turns + 2 |> modBy 4 }

                    else
                        { edge | colors = reverseArray edge.colors }
                )
    , image = flipMatrixAroundXAxis White tile.image
    , imageWithBorders = flipMatrixAroundXAxis White tile.imageWithBorders
    }


flipTileAroundYAxis : Tile -> Tile
flipTileAroundYAxis tile =
    { edges =
        tile.edges
            |> List.map
                (\edge ->
                    if edge.turns |> modBy 2 |> (==) 0 then
                        { edge | colors = reverseArray edge.colors }

                    else
                        { edge | turns = edge.turns + 2 |> modBy 4 }
                )
    , image = flipMatrixAroundYAxis White tile.image
    , imageWithBorders = flipMatrixAroundYAxis White tile.imageWithBorders
    }


rotateTile : Int -> Tile -> Tile
rotateTile turns tile =
    { edges =
        tile.edges
            |> List.map
                (\edge ->
                    { edge | turns = edge.turns + turns |> modBy 4 }
                )
    , image = rotateMatrix turns White tile.image
    , imageWithBorders = rotateMatrix turns White tile.imageWithBorders
    }


flipMatrixAroundXAxis : a -> Matrix a -> Matrix a
flipMatrixAroundXAxis default matrix =
    List.range 0 (Matrix.height matrix - 1)
        |> List.filterMap
            (\y ->
                Matrix.getRow y matrix
                    |> Result.toMaybe
                    |> Maybe.map (arrayToRow default)
            )
        |> List.foldl
            (Result.andThen << Matrix.concatVertical)
            (Matrix.repeat (Matrix.width matrix) 0 default |> Ok)
        |> Result.toMaybe
        |> Maybe.withDefault (Matrix.repeat 0 0 default)


flipMatrixAroundYAxis : a -> Matrix a -> Matrix a
flipMatrixAroundYAxis default matrix =
    List.range 0 (Matrix.width matrix - 1)
        |> List.filterMap
            (\x ->
                Matrix.getColumn x matrix
                    |> Result.toMaybe
                    |> Maybe.map (arrayToColumn default)
            )
        |> List.foldl
            (Result.andThen << Matrix.concatHorizontal)
            (Matrix.repeat 0 (Matrix.height matrix) default |> Ok)
        |> Result.toMaybe
        |> Maybe.withDefault (Matrix.repeat 0 0 default)


rotateMatrix : Int -> a -> Matrix a -> Matrix a
rotateMatrix turns default matrix =
    case turns |> modBy 4 of
        0 ->
            matrix

        1 ->
            List.range 0 (Matrix.height matrix - 1)
                |> List.filterMap
                    (\y ->
                        Matrix.getRow y matrix
                            |> Result.toMaybe
                            |> Maybe.map (arrayToColumn default)
                    )
                |> List.foldl
                    (Result.andThen << Matrix.concatHorizontal)
                    (Matrix.repeat 0 (Matrix.width matrix) default |> Ok)
                |> Result.toMaybe
                |> Maybe.withDefault (Matrix.repeat 0 0 default)

        2 ->
            List.range 0 (Matrix.height matrix - 1)
                |> List.filterMap
                    (\y ->
                        Matrix.getRow y matrix
                            |> Result.toMaybe
                            |> Maybe.map (reverseArray >> arrayToRow default)
                    )
                |> List.foldl
                    (Result.andThen << Matrix.concatVertical)
                    (Matrix.repeat (Matrix.width matrix) 0 default |> Ok)
                |> Result.toMaybe
                |> Maybe.withDefault (Matrix.repeat 0 0 default)

        3 ->
            List.range 0 (Matrix.height matrix - 1)
                |> List.filterMap
                    (\y ->
                        Matrix.getRow y matrix
                            |> Result.toMaybe
                            |> Maybe.map (reverseArray >> arrayToColumn default)
                    )
                |> List.foldr
                    (Result.andThen << Matrix.concatHorizontal)
                    (Matrix.repeat 0 (Matrix.width matrix) default |> Ok)
                |> Result.toMaybe
                |> Maybe.withDefault (Matrix.repeat 0 0 default)

        _ ->
            matrix


arrayToColumn : a -> Array a -> Matrix a
arrayToColumn default array =
    Matrix.generate
        1
        (Array.length array)
        (\_ y -> Array.get y array |> Maybe.withDefault default)


arrayToRow : a -> Array a -> Matrix a
arrayToRow default array =
    Matrix.generate
        (Array.length array)
        1
        (\x _ -> Array.get x array |> Maybe.withDefault default)



-- matches : Array Color -> Tile -> Int
-- matches wantedEdge tile =
--     let
--         edges =
--             [ Matrix.getRow 0 tile.matrix
--             , Matrix.getRow (size - 1) tile.matrix
--             , Matrix.getColumn 0 tile.matrix
--             , Matrix.getColumn (size - 1) tile.matrix
--             ]
--                 |> List.filterMap Result.toMaybe
--                 |> List.concatMap
--                     (\edge ->
--                         [ edge
--                         , edge |> Array.toList |> List.reverse |> Array.fromList
--                         ]
--                     )
--     in
--     edges
--         |> List.filter ((==) wantedEdge)
--         |> List.length


main : Html Never
main =
    case puzzleInput |> parse |> Result.andThen puzzle of
        Ok ( cornerIdProduct, image, imageWithBorders ) ->
            Html.div []
                [ Html.text (String.fromInt cornerIdProduct)
                , viewImage image
                , viewImage imageWithBorders
                ]

        Err message ->
            Html.text message


viewImage : Matrix Color -> Html msg
viewImage matrix =
    Html.pre [ Html.Attributes.style "font-size" "16px" ]
        [ Html.text
            (matrix
                |> Matrix.indexedMap (\x y color -> ( x, y, color ))
                |> Matrix.foldl
                    (\( x, y, color ) string ->
                        let
                            newline =
                                if x == 0 then
                                    "\n"

                                else
                                    ""
                        in
                        string ++ newline ++ colorToString color
                    )
                    ""
            )
        ]


colorToString : Color -> String
colorToString color =
    case color of
        White ->
            "."

        Black ->
            "#"



-- Html.div []
--     [ showResult (solution1 puzzleInput)
-- , case parse puzzleInput of
--     Ok tiles ->
--         tiles
--             |> List.map
--                 (\tile ->
--                     let
--                         others =
--                             tiles |> List.filter (\tile2 -> tile2.id /= tile.id)
--                         edges =
--                             [ ( "Top", Matrix.getRow 0 tile.matrix )
--                             , ( "Bottom", Matrix.getRow (size - 1) tile.matrix )
--                             , ( "Left", Matrix.getColumn 0 tile.matrix )
--                             , ( "Right", Matrix.getColumn (size - 1) tile.matrix )
--                             ]
--                                 |> List.filterMap
--                                     (\( text, result ) ->
--                                         result |> Result.toMaybe |> Maybe.map (Tuple.pair text)
--                                     )
--                                 |> List.map
--                                     (Tuple.mapSecond
--                                         (\edge ->
--                                             others
--                                                 |> List.map (matches edge)
--                                                 |> List.filter (\n -> n > 0)
--                                                 |> List.length
--                                         )
--                                     )
--                     in
--                     ( tile, edges )
--                 )
--             |> List.filter
--                 (\( _, edges ) ->
--                     edges
--                         |> List.filter (\( _, n ) -> n == 0)
--                         |> List.length
--                         |> (\n -> n >= 2)
--                 )
--             |> (\list ->
--                     let
--                         _ =
--                             Debug.log "product" (list |> List.map (Tuple.first >> .id) |> List.product)
--                     in
--                     list
--                )
--             |> List.map
--                 (\( tile, edges ) ->
--                     Html.div [ Html.Attributes.style "margin-top" "24px" ]
--                         [ Html.div [ Html.Attributes.style "font-weight" "bold" ]
--                             [ Html.text (String.fromInt tile.id) ]
--                         , edges
--                             |> List.map (\( text, num ) -> Html.div [] [ Html.text (text ++ ": " ++ String.fromInt num) ])
--                             |> Html.div []
--                         ]
--                 )
--             |> Html.div []
--     Err message ->
--         Html.text message
-- ]


showResult : Result String a -> Html msg
showResult result =
    Html.output []
        [ Html.text
            (case result of
                Ok int ->
                    Debug.toString int

                Err error ->
                    error
            )
        ]


shortInput : String
shortInput =
    """
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
"""

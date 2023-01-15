module Day20 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import LineParser
import List.Extra as List
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
            in
            if Dict.isEmpty tilesLeft then
                Ok
                    ( cornerIdProduct
                    , joinImage (originalSize - 2) .image
                    , joinImage (originalSize + 2) .imageWithBorders
                    )

            else
                Err
                    ("All tiles were not consumed: "
                        ++ (tilesLeft |> Dict.toList |> List.map (Tuple.first >> String.fromInt) |> String.join ", ")
                    )


puzzleHelper : ( Int, Int ) -> Int -> Tile -> ( Dict Int Tile, Dict ( Int, Int ) ( Int, Tile ) ) -> ( Dict Int Tile, Dict ( Int, Int ) ( Int, Tile ) )
puzzleHelper ( x, y ) tileId tile ( initialTilesLeft, initialResult ) =
    let
        nextTiles =
            tile.edges
                |> List.filterMap
                    (\edge ->
                        findNextTile edge initialTilesLeft
                            |> Maybe.map (Tuple.pair edge)
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
                        { edge
                            | turns = edge.turns + 2 |> modBy 4
                            , colors = reverseArray edge.colors
                        }

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
                        { edge
                            | turns = edge.turns + 2 |> modBy 4
                            , colors = reverseArray edge.colors
                        }
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


type Spot
    = Calm
    | Rough
    | SeaMonster


findSeaMonsters : Matrix Color -> List ( Int, Matrix Spot )
findSeaMonsters image =
    List.range 0 3
        |> List.concatMap
            (\turns ->
                [ ( turns, identity )
                , ( turns, flipMatrixAroundXAxis White )
                , ( turns, flipMatrixAroundYAxis White )
                , ( turns, flipMatrixAroundXAxis White >> flipMatrixAroundYAxis White )
                ]
            )
        |> List.filterMap
            (\( turns, flip ) ->
                let
                    seaMonsterImage =
                        image
                            |> rotateMatrix turns White
                            |> flip
                            |> toSeaMonsterImage

                    array =
                        Matrix.toArray seaMonsterImage

                    numRough =
                        array
                            |> Array.filter ((==) Rough)
                            |> Array.length

                    numSeaMonster =
                        array
                            |> Array.filter ((==) SeaMonster)
                            |> Array.length
                in
                if numSeaMonster > 0 then
                    Just ( numRough, seaMonsterImage )

                else
                    Nothing
            )


seaMonsterPattern : List ( Int, Int )
seaMonsterPattern =
    """
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """
        |> String.lines
        |> List.drop 1
        |> List.indexedMap
            (\y ->
                String.toList
                    >> List.indexedMap
                        (\x char ->
                            if char == '#' then
                                Just ( x, y )

                            else
                                Nothing
                        )
                    >> List.filterMap identity
            )
        |> List.concat


toSeaMonsterImage : Matrix Color -> Matrix Spot
toSeaMonsterImage image =
    image
        |> Matrix.indexedMap (\x y color -> ( x, y, color ))
        |> Matrix.foldl
            (\( x, y, color ) seaMonsterImage ->
                let
                    coords =
                        seaMonsterPattern
                            |> List.map (\( dx, dy ) -> ( x + dx, y + dy ))

                    isSeaMonster =
                        coords
                            |> List.all
                                (\( cx, cy ) ->
                                    case Matrix.get cx cy image of
                                        Ok White ->
                                            False

                                        Ok Black ->
                                            True

                                        Err _ ->
                                            False
                                )
                in
                if isSeaMonster then
                    coords
                        |> List.foldl
                            (\( cx, cy ) ->
                                Matrix.set cx cy SeaMonster
                            )
                            seaMonsterImage

                else
                    seaMonsterImage
            )
            (Matrix.map colorToSpot image)


colorToSpot : Color -> Spot
colorToSpot color =
    case color of
        White ->
            Calm

        Black ->
            Rough


main : Html Never
main =
    case puzzleInput |> parse |> Result.andThen puzzle of
        Ok ( cornerIdProduct, image, imageWithBorders ) ->
            Html.div []
                [ Html.text (String.fromInt cornerIdProduct)
                , Html.div []
                    (case findSeaMonsters image of
                        [] ->
                            [ Html.text "No sea monsters found." ]

                        seaMonsterImages ->
                            seaMonsterImages
                                |> List.uniqueBy Tuple.first
                                |> List.map
                                    (\( numRough, seaMonsterImage ) ->
                                        Html.div []
                                            [ Html.text (String.fromInt numRough)
                                            , viewImage spotToString seaMonsterImage
                                            ]
                                    )
                    )
                , viewImage colorToString image
                , viewImage colorToString imageWithBorders
                ]

        Err message ->
            Html.text message


viewImage : (a -> String) -> Matrix a -> Html msg
viewImage toString matrix =
    Html.pre [ Html.Attributes.style "font-size" "16px" ]
        [ Html.text
            (matrix
                |> Matrix.indexedMap (\x y color -> ( x, y, color ))
                |> Matrix.foldl
                    (\( x, y, color ) string ->
                        let
                            newline =
                                if x == 0 && y /= 0 then
                                    "\n"

                                else
                                    ""
                        in
                        string ++ newline ++ toString color
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


spotToString : Spot -> String
spotToString spot =
    case spot of
        Calm ->
            "."

        Rough ->
            "~"

        SeaMonster ->
            "O"


puzzleInput : String
puzzleInput =
    """
Tile 1487:
.##..#...#
##.##.....
...#....##
##..#..#.#
.####....#
.#.#..#..#
..##.....#
..##....##
#.....#..#
...#..####

Tile 3637:
##...#....
#.#..#...#
....##...#
#.........
#......#.#
##...#.#..
#.........
##.....###
##.#......
##.#.##..#

Tile 3433:
.....#..#.
##..##...#
....#.....
.........#
#........#
#..##..###
#..#..##..
#...##.#..
.#...#..#.
#.##....#.

Tile 3931:
#....##..#
...#..###.
#...#..#.#
.......#..
#.......#.
###......#
###.#.#..#
.##..#..#.
...##..#.#
..##..#.#.

Tile 2213:
.###...##.
..##......
..........
.##...#...
...##...#.
..#.#..#.#
..##......
#..#.#.###
...#......
#.#..##...

Tile 1901:
.#.##.#.#.
#.........
#.##..#.##
...###...#
.....#.#.#
.#.....#..
....#.#...
..#...#..#
#........#
####.###..

Tile 3917:
.##.#..#.#
#.##....#.
#.....#...
#.#...#.##
..........
..........
.#....#.#.
.........#
.....#....
#.#.##....

Tile 1831:
.#..#..##.
#.....##..
###....###
.#.#.##..#
#....#...#
......#.#.
.....#..##
#..##.....
####.#.##.
...##.#..#

Tile 3557:
..#.#.#.#.
##........
.......#..
.##.......
..........
....#..#..
#...#....#
#..#####.#
#..#...##.
#.....###.

Tile 2659:
####.##.#.
.##.##...#
####..#...
##........
####......
.......#.#
###....#..
.....#..##
#....#...#
.#..###.#.

Tile 3061:
.#..#.#..#
........#.
##.#....#.
###....#..
.........#
..#..#....
....#..#.#
.#.##...##
##.#....##
.#.#.##.##

Tile 3593:
###.#.####
.#........
#..#....#.
.......###
.#....##.#
....#..##.
#.#.....##
#..#...#..
.#......#.
#....##..#

Tile 2719:
..###.#..#
..##...##.
..##.....#
#.......#.
...#....##
#.#....#..
#.......#.
#..#.....#
........##
.#.#..##.#

Tile 3089:
##.#.#.#.#
.#..#....#
.#.#.....#
##...#.#.#
.......#..
###..####.
....#.....
........#.
.#.....##.
..####..#.

Tile 1723:
..#.##.###
#..###.#.#
..#..#...#
....#.#...
....#....#
#....#....
#.....#...
#.#..#.#..
#...###..#
###.####..

Tile 2297:
.##....#.#
####..#.##
#.##...##.
#.##...#.#
###.##..##
....#...##
##....##.#
..##...##.
##.#.#..#.
#...#.##..

Tile 3659:
.#####....
....#.#.##
.#..#....#
....#....#
.....#...#
.....#..##
##..#.....
##...#....
####....##
##......#.

Tile 1847:
###.#.####
..##.....#
#.......#.
##......#.
..#.#.....
#.......#.
.#..#...#.
.#......#.
#.....###.
##.###.##.

Tile 1601:
#.#.##.###
.#..#...#.
##...#..#.
#.#####..#
.......#.#
##..##.##.
..#.#..#..
..###....#
.....#...#
....#####.

Tile 3323:
..#..##.##
##......##
###.#...#.
.#..#..#..
#...#..###
#....##...
##.....#.#
...####...
....#.....
..##....##

Tile 1321:
###.######
#...#.#..#
##........
#..#..#.#.
..##.#..#.
.........#
.#.......#
#..#......
##.#.....#
.###...#.#

Tile 2551:
....##...#
...#.##...
.##.......
...#..#...
#.#...##.#
#..##.#...
.##..#.##.
##....#..#
...#.....#
..#.#.#.#.

Tile 2503:
...#..##.#
.........#
#.#....##.
#.........
##..#.#...
#...#..#..
...#......
.##.##....
#.........
###.....##

Tile 1163:
#.####..#.
.##.#.#...
#.#..#####
......#...
##....#..#
#.......##
#.#.##..#.
##...###.#
#.........
.##.#...##

Tile 1039:
#.###.#.#.
.##......#
.#...#..#.
.#...#...#
#.#...#..#
##.#...#..
#.##......
#.#.#..#..
##...#..##
#.####..##

Tile 2953:
...#.#.#.#
..#.#.....
.........#
#..#...#.#
......#...
.........#
....#..#..
.###....#.
##.##..###
..####.#..

Tile 1609:
...#...#..
#....##..#
...###.#..
#.##.##..#
#.#.##.#..
##.......#
.##...#..#
#....#....
........#.
.....#..#.

Tile 1187:
####.##...
..........
.....#...#
#.#......#
#....#.###
.#..###.#.
##...#....
......#..#
....#.#..#
#####...##

Tile 3313:
#######..#
#.#...####
..........
#.#..##...
###......#
##........
##......##
##..#.#...
..#..#..#.
#..#...##.

Tile 3221:
....#.#.#.
##..#.....
.#.##..#..
#..##..#.#
....#....#
##.......#
#.......#.
#.###...##
#..#......
#....##...

Tile 2647:
.##.#.####
..#.......
#...##....
#..#....##
......#...
..##......
.#.....#.#
###.##....
###...###.
##.#.#..#.

Tile 2879:
#.#.#..###
......####
##....#.#.
.#.......#
....##..#.
#..#.###.#
#.........
##.......#
#...##...#
..#...##.#

Tile 2423:
.###.#..#.
..###.##..
..#.#...##
#....#...#
#..#.##...
#..#....##
.....#....
##.##....#
....#....#
...#..#.#.

Tile 2557:
.#..#..##.
#....##.##
...#.#...#
#...####..
#........#
...#....##
..........
.##..#.##.
#.#..#...#
.###..#.#.

Tile 2819:
..#......#
#....#....
##..#...##
#......###
#..##.....
#.###.#...
.##...####
..........
#........#
#...##.#.#

Tile 2531:
....#.#..#
..#..#..#.
#....#...#
#..#..#..#
.........#
#.#.......
#......###
#.#...##..
#....###.#
.#.#####.#

Tile 1109:
..#.#.#.##
.....#....
#...##.#.#
##.##.#...
.........#
.#..#.#..#
#.........
.##...#..#
#.#..#..#.
.#.###.#..

Tile 1103:
#..##.#...
.###......
#..#.....#
.....###.#
.#..#.##.#
......##.#
#...##.#..
...#...###
.##..#.##.
.#.#.#..#.

Tile 2713:
.###.##...
#.##...#.#
.....#...#
#.........
##...#..##
.###.#.##.
#.#..##.##
#...#..##.
...#...##.
.....#.##.

Tile 2927:
##..#...#.
...#..#..#
#..#..#...
.....#.#..
##.......#
...##.##.#
#.#.......
.###....#.
....#.####
####.###.#

Tile 3319:
....#.#.##
#.###..###
.#....#.#.
.....#..#.
.##....#..
....#...##
#.#...#...
.......#..
#.#.###.#.
......##..

Tile 2311:
#.#.##...#
..#....#..
..#..#..#.
..#.#.#.##
......####
..#..#..##
##....##.#
.#..#.....
.....##..#
...##.....

Tile 1549:
##.#.#.#.#
##..#...#.
.#....#...
.#....##.#
..#..###.#
#....#..##
.#.......#
#.....#.##
.....#....
#.#...#.#.

Tile 3631:
...#.###.#
........#.
###..##..#
###.....#.
##....##.#
....#.....
###.#.#...
...#..#...
##..#....#
....##..#.

Tile 1787:
#.##.##.#.
....#...#.
#..#..#..#
...#.....#
#.##.#....
##.......#
#.....#...
#...##.#..
.#.##.#..#
###.#....#

Tile 3467:
########.#
##...#.#.#
...#......
.....#.#.#
#.....#..#
#........#
...#......
..#.....##
.#.##..#..
..#.....#.

Tile 3373:
##..#.#..#
...#......
###.##..#.
#........#
.#...#...#
#....##...
.#.....#..
.#....#..#
#.#..####.
...####.#.

Tile 3911:
#.###.#..#
.##......#
...#......
#...#..##.
.......#.#
#.#......#
...##..#.#
##..#..#.#
#......##.
#####..###

Tile 1327:
...#.#..#.
.........#
#..#...#..
#........#
###.#.....
.........#
##.#......
#........#
#..#...#.#
.#.##..##.

Tile 3643:
.#.#...#.#
#.##....#.
#..#...#..
....##....
#....##.##
..........
#...#.#.##
##.#..#..#
...#....##
..##..#..#

Tile 2447:
#...#.###.
#.....#..#
#.#.......
###..#...#
.....##.##
#....#...#
#.#.#.....
#.....#...
...##.....
.#.###.##.

Tile 3181:
.###..#...
##...#.#.#
#..#......
#......#.#
.#...##.#.
#.#......#
.........#
.........#
#.#...#...
##.##.#.#.

Tile 3037:
#########.
...#.#...#
.##......#
.#........
#....#...#
#..#..#.#.
...###.#.#
#.#...#.##
##..#.#..#
#..#.##.#.

Tile 3671:
##.#.#....
........#.
##.#....#.
#.##.#.#.#
##....##..
.##...#.#.
.#.#...#..
##...##..#
#.#....###
##.#..###.

Tile 2909:
.##...###.
#.....#..#
.#..#.....
#.###.#.##
#.##.#.##.
..#......#
.#..#.#...
#..#..#.#.
.#.#...#..
.###..##.#

Tile 3571:
..#.#.#..#
#....#....
.....##..#
.....###.#
....#.#...
......#..#
#.......##
..........
..#.##..#.
##.##.##.#

Tile 1931:
###.##...#
#...#.#...
#..#..#.##
..##......
...##....#
#.#.##..##
#.##.....#
#..#......
..#..#.#.#
....##.#.#

Tile 2689:
#....#.##.
....#...##
......##..
#....#...#
........#.
..........
##.#.###..
..#.......
....##....
...#.#..##

Tile 3499:
.#.###.#.#
......#..#
..#......#
.#......#.
##.......#
##...##...
.#......#.
#......#..
#....#....
###.######

Tile 1277:
...##.#...
......#..#
###......#
#.........
..##.....#
#.#....###
#.........
#..#.....#
#.........
###.#.#...

Tile 2633:
.#..#.##..
#........#
......#..#
..#.......
.##....#.#
#.......#.
.#.##.....
#.........
........#.
##.#.#####

Tile 1699:
.##.#..##.
#..##.....
...#...##.
..#....#..
#..#..##.#
..#.....##
.##......#
..#....#..
#..###..#.
#..#.####.

Tile 2339:
.#.#####..
##.#...#..
..##.#....
.###....#.
#..##....#
#.........
.....#....
#...#....#
#..##...#.
#....##...

Tile 1993:
.####.....
.....#..#.
..#.#..#.#
#........#
...##.#.#.
..#.#.#..#
.###.##.#.
..#.#..#..
....#####.
..#.#...##

Tile 2467:
.####..#.#
#####.....
##...##...
#....##.##
.....###..
#....###.#
......#..#
....##.#..
....##.#..
..#...#.#.

Tile 2153:
.##.##.#.#
.........#
....#..##.
...#......
#..#....##
....#....#
..##.#...#
........#.
##..#.##.#
..#.##....

Tile 1867:
.#....####
#...#....#
....#..#..
.#.....###
....#..#.#
#..#..#..#
....#...#.
.....#...#
.....##...
.##..#.#.#

Tile 2111:
..#.#..###
.....#....
..#.#.##..
.#.......#
..#.......
..#..#.#..
.#.....#..
....#....#
##.#...#..
####.##...

Tile 3307:
.#.#..####
###..#.#..
........##
.....#..#.
.#....##..
###.#....#
.#........
.......#..
#.#.....##
..#.#..##.

Tile 1061:
##.##.....
#..#.#..#.
..#....#..
#.....#..#
#.#..#...#
#.....####
#....#....
##......##
......##.#
##...##...

Tile 1021:
##.###.##.
.#....#..#
#.....#..#
#..#..##.#
##.....#.#
#.#....##.
..#.....#.
.#.....#.#
.#..#.....
.....#.##.

Tile 3677:
.#...#...#
..........
....###...
.######..#
..#...#...
#...#.#...
##.......#
..#......#
.#..#....#
#..##.####

Tile 1231:
#.###.##..
..#.......
#..#.....#
#..#.....#
.#.....#.#
##......##
.#.###....
..#....###
##.....#.#
##.....#..

Tile 3301:
.........#
.#..#....#
##.#...#.#
..#..#...#
.##....##.
..#...#...
##.##....#
#..#.#....
.....#..##
#...#..###

Tile 2861:
.....#.###
.#####.###
###.#...#.
##..###..#
..........
##...#.#..
##..#..#..
.##..#...#
#.....#...
.....###..

Tile 3823:
.##.##.###
..#.#.....
#.#####.#.
.#......##
#.#..#.#.#
...#..##.#
##..##...#
....#..#..
....#....#
##.#......

Tile 2027:
.#.#..#..#
....#.....
.#..##....
#..###..##
#...#.....
#...#...##
.#........
##.##.#...
.....#.#..
###..##.##

Tile 3673:
....####.#
.#...###.#
....#..#..
.#.#.#....
..#..#....
#....##.#.
.....#....
#...#....#
#..#.#....
###.#.#...

Tile 1777:
.####.####
#..#....#.
....###...
.....#.#..
....#.....
.#........
##.#.....#
.#.......#
.#...#...#
.####.#.##

Tile 2609:
.#.#####..
......#..#
#.#.#.....
..##...#..
....#....#
#....#...#
..#.#...##
#...#...##
#..#.....#
###..#....

Tile 2687:
##...#.#.#
.####.....
#.#..#..#.
#...##...#
#....#.##.
.##......#
#..##..#..
.#.#.#..#.
#.#.##..##
##..#...#.

Tile 1451:
######.#..
##.###...#
...##.....
#..##...#.
.....#..#.
#..#..#...
..#...#...
##..##...#
#......##.
..#..#.#..

Tile 3251:
.####..##.
#.........
##..###...
#......#..
##..##..#.
..........
#........#
#........#
..##.#.###
........#.

Tile 3391:
..#######.
#..#.#...#
.......#..
#...#....#
#.....#..#
##........
#.....#..#
.....#..##
..###...##
.#..#.#.#.

Tile 1129:
.##..#.#..
##.#.#....
###...#..#
..........
......####
..........
##...#...#
#.#...#.##
#.........
###...####

Tile 2087:
..###.#...
#...##...#
...#.....#
..##...#..
##.....#.#
####.....#
..#.#..#..
#.#.##....
..###....#
.#.#.##.#.

Tile 1361:
#...######
...#......
#.#.#.#..#
#.....#.##
....#...##
#.......##
#......#..
#.....#..#
..#....#.#
.....##.#.

Tile 1583:
#.#..##.#.
#..#.#....
#...#..#..
##.....#.#
#...#.#...
...###....
..#..#.##.
...####..#
#....#....
#..#.##.#.

Tile 3361:
.###.#####
.##..#...#
.....##...
#.#....#..
#..#...#..
.........#
#........#
..#.......
#.###.....
#...###...

Tile 3463:
.....###.#
...##.#..#
.#...#....
###.##..#.
#.###...#.
#..#..#...
#..##.....
#..#...#.#
#........#
.###..#.#.

Tile 1997:
#.##....#.
..#...#...
#........#
.....#.#.#
.#...#....
#..#.#....
##...#....
##.....#.#
#.........
#.......##

Tile 2131:
.##.##.#..
...#.#...#
###.####.#
......###.
.#.#..#.##
..#..#.###
......#.##
..##..#..#
...#.#....
###.####..

Tile 3943:
###..#####
.#.#...#.#
##..#.#.##
#..#....##
#...#....#
#..#..##.#
..#....##.
#.......##
#.........
..##...##.

Tile 1481:
.###...#.#
####.##.#.
.###.##.#.
...#.#...#
#...#....#
.#.....#..
..##....##
##.##..###
.....#...#
###.##.#.#

Tile 3191:
..#.###.##
#...#...#.
#....#....
..#.....#.
...#..#.##
........##
#..#......
#.........
.#........
.......##.

Tile 3833:
......#...
.......#..
......#.#.
...#.....#
...##.#.#.
.#.....###
#..##..#..
....#....#
.#..#..#..
#.##..#.#.

Tile 1627:
###...####
#.........
#..#..###.
.....##..#
#....#....
#.##..##.#
....#..##.
...##...#.
.###..#...
..#.##.###

Tile 2801:
#####.###.
..#.##.###
.##...##.#
#.......##
.......#..
..........
......#..#
#.....#..#
##..#.###.
.#..#.#.##

Tile 1889:
##.#....##
..###....#
#..#.....#
.#......##
##..#.#...
....#...##
......#...
#.##....#.
...##..#..
.##.#.#...

Tile 1823:
....#.#.#.
.#........
#...##...#
...####..#
...#...###
#...#..#..
#.#..#....
#...#..#.#
......#...
###.###.#.

Tile 2663:
##.##.#.##
.#.#.....#
.#.##....#
#....#...#
#..#..##..
#.###...#.
.......#.#
#..#......
.....#....
.##...#...

Tile 1091:
.##...#.##
#.....#...
####.....#
.......#..
...#......
#.#..#...#
#.#.....##
##......#.
..#.......
.#.#...##.

Tile 3163:
#####..#.#
####..#.#.
.##.#..###
...#....#.
.........#
..#......#
##.....#.#
#.....#..#
#..#...#.#
.####..##.

Tile 1031:
.#.##.#...
##.##..#.#
.....#....
#..#......
......#...
.....#.#..
.....#.#.#
.#...#....
.##....##.
.#.##....#

Tile 2269:
####.####.
#.........
#...##.##.
..##..#..#
...#......
.#....#..#
#..##....#
..........
..#.....#.
.####...##

Tile 3793:
.##.#.###.
.##.##..##
..#...#.#.
......#..#
..####....
#........#
#....#...#
#....#.#..
.#.#..###.
#...#.###.

Tile 3079:
#.#.#.#...
##..#....#
..#...#..#
#..###....
#.#.......
..#.#.....
....#.....
###.##.###
...##.....
...###...#

Tile 3187:
.##.##.###
.#.....#.#
..##.##.#.
#.###.....
.##.##..##
#..#.....#
#...#.#..#
..#.#...##
#........#
...##.#...

Tile 1553:
.###.#..#.
.#..##.#..
#....#...#
#....##..#
#.#.##....
....#...#.
.........#
#..##..#..
.#......#.
.###.#.#..

Tile 1049:
##.#.#.###
...###.#.#
...##.....
#...#.#.#.
....##....
.##....#..
.....#.#.#
..#..#..#.
..#.#....#
....#..##.

Tile 2083:
.#......##
...#......
....#.#...
...##.#..#
#...##...#
.....#...#
..##.#...#
#....#..##
.....#....
...#..#.#.

Tile 3049:
.#...##.#.
.#..#....#
....#.##..
##..#.....
.........#
#......#..
...#.#....
...#.##.#.
.##...#..#
.##.##.#.#

Tile 3167:
..##...###
#....#..#.
#.#..#....
#.........
#..##..#.#
..........
.....#..##
.#.....##.
##......##
.##....#..

Tile 1693:
.#.###..##
#.........
##...#....
..#....#.#
...#......
...#.#...#
.#.....###
.....#####
...##....#
.#.##.....

Tile 2473:
....#.##.#
..#......#
..#......#
....#..#.#
#...#.....
..#..#..#.
.##..###.#
#.#..#....
#....##...
#.#..#.###

Tile 3821:
.#.#####.#
#.......#.
#..#.#..##
##........
.#.....#.#
.....#....
.#.#.....#
..#..##...
##..#.....
##.....###

Tile 3529:
........#.
.#....##.#
#......##.
#..#...#.#
##........
#.#...#...
...#..#.##
.#.#..#..#
#..####..#
.##..##..#

Tile 1801:
#####.#.##
#.#.......
#.#.#####.
##.#.....#
....##.#.#
#..#.#....
.#........
#.#.##...#
#.#......#
..#..#...#

Tile 3709:
.##....#..
........#.
....#.....
##..#.#...
..#..#.#..
..#.#.....
#.........
#.#.##..##
....##....
#.#.#....#

Tile 2707:
###..##.##
.#.####..#
#.#.##.#.#
#...#.#...
.......###
.....##...
...####...
###...#...
.........#
#.###.##..

Tile 3733:
##...#.##.
.....#....
#......##.
##........
##...##...
#.#...#...
#......#.#
#.#....#.#
#...#.#.##
.#..#...#.

Tile 1663:
#.#.#.###.
....##...#
...#...#.#
##...####.
.#........
##....#..#
.....#...#
#..#...#..
#######..#
##.#.#.##.

Tile 2309:
..##..#...
...#.....#
########.#
#....##..#
.#..#....#
..#.#....#
##.....#.#
..........
.#....#.##
##..#.###.

Tile 1483:
..#..#...#
##.#.#....
#.#....###
##..#.####
##.#.#.#.#
..#.#..##.
..##.....#
..#....###
#..##....#
..####..##

Tile 2591:
.#.####.##
.#.#.....#
#..##..#..
#...#.#..#
##.##.#...
.....###..
#....##..#
#....#..#.
##.#..##..
..#..#.#..

Tile 2671:
...#.###.#
#.#.....#.
.#.....#..
...##.####
.#.###..##
.#.#.#.#..
#...##..#.
#.......##
.#....##.#
.#..##..##

Tile 1051:
#.#.#.....
..#..#.##.
......#..#
#.#.......
..#.#.#..#
.#.##.#..#
#..#.###.#
#...#..###
#.......#.
...#...###

Tile 1613:
..##...#.#
#......#..
#.#..#..##
.#.#.#....
#.####.###
##..##.#..
##........
.#.#...#..
#.#....#..
.##...#.##

Tile 3457:
##.#.##..#
##.#...#.#
#..#...#.#
###....#..
.#....#...
#...##..##
..........
.........#
..#..#..#.
..#..####.

Tile 3739:
...######.
#.#.#..##.
#.#.....#.
#..#..#.##
#..#......
#.#......#
#..##.....
#.##.#....
#.#..#..#.
#.#.#..##.

Tile 2069:
....#.###.
#.#...#.#.
.##.......
..#.#.....
..........
#...#..#..
..........
###...#...
#...#.###.
.##.##..##

Tile 3623:
##..##....
#..#.#....
.....#....
#........#
.#.....#..
##..#.#.#.
#....#...#
..........
#.##.....#
#.....#..#

Tile 3613:
..#.##..#.
#...##....
.........#
#......#..
....####.#
..#.##....
..#.#...##
#.#..#....
#.#...#...
.#####.#..

Tile 3329:
##....#.#.
....#..##.
..##..#.#.
.#.##..#..
#.#.#..#..
.........#
.......#..
#..###...#
###.#.#.#.
##.#.#...#

Tile 3209:
.#.....##.
....####..
..........
..##...#..
....#...#.
#...##.#.#
#.##.###..
#...#....#
##........
..#.#####.

Tile 2351:
#....#####
#.....#...
.....#...#
#.#..#....
.#.###...#
.#...##..#
#.#......#
#....##.#.
...#....##
#####...#.

Tile 3011:
#.###....#
#...#.#..#
........##
#.....#...
..#...#.#.
......#.##
.....##..#
.##..#...#
#...#.....
#######.##

Tile 1409:
#..#..#.#.
....#..#..
#...#...##
#..#.#....
...##..#.#
#...#.....
...#.##.##
#..##.....
.#......##
.#######.#

Tile 2341:
.#.....##.
.##..#...#
.#..#.....
#........#
#..#.....#
###.......
##.##.#...
.......#.#
#.#...#...
##.#.##...

Tile 1747:
#.....#...
..##...#.#
...###....
.........#
.#..##.#..
#.##.....#
#.###..#..
#.#......#
##..##....
#.####.#.#

Tile 3881:
...###....
.........#
#...##....
#...##...#
#.#.#.#...
..#..##.#.
#....#....
#....##.##
...#......
####.##.#.

Tile 1307:
#####.##..
#...#..###
..........
##.#.....#
..#.#..##.
....#.#.#.
..........
#.........
.#.....#..
.#.#.####.

Tile 3779:
#.....####
....#...##
#...#...#.
#.....##..
#.##....#.
#..#....##
.......#..
#.#.#....#
.#..#.#...
.##..#....

Tile 2957:
.....##..#
..#....#.#
#..#...##.
.......###
...#..#.##
##......#.
.#.....##.
#...##.##.
#....#.###
..##.#..##
"""

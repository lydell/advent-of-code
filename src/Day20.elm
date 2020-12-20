module Day20 exposing (..)

import Array exposing (Array)
import Day20Input exposing (puzzleInput)
import Html exposing (Html)
import Html.Attributes
import LineParser
import List
import Matrix exposing (Matrix)
import Matrix.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Result


size : Int
size =
    10


type alias Tile =
    { id : Int
    , matrix : Matrix Color
    }


type Color
    = White
    | Black


parse : String -> Result String (List Tile)
parse =
    String.trim
        >> String.split "\n\n"
        >> LineParser.parseGeneral "Tile"
            identity
            (Parser.run tileParser
                >> Result.mapError Parser.Extra.deadEndsToString
            )


tileParser : Parser Tile
tileParser =
    Parser.succeed Tile
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
                        if Matrix.width matrix /= size || Matrix.width matrix /= size then
                            Parser.problem
                                ("Expected a "
                                    ++ String.fromInt size
                                    ++ "x"
                                    ++ String.fromInt size
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


solution1 : String -> Result String Int
solution1 input =
    Ok 42


matches : Array Color -> Tile -> Int
matches wantedEdge tile =
    let
        edges =
            [ Matrix.getRow 0 tile.matrix
            , Matrix.getRow (size - 1) tile.matrix
            , Matrix.getColumn 0 tile.matrix
            , Matrix.getColumn (size - 1) tile.matrix
            ]
                |> List.filterMap Result.toMaybe
                |> List.concatMap
                    (\edge ->
                        [ edge
                        , edge |> Array.toList |> List.reverse |> Array.fromList
                        ]
                    )
    in
    edges
        |> List.filter ((==) wantedEdge)
        |> List.length


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
        , case parse puzzleInput of
            Ok tiles ->
                tiles
                    |> List.map
                        (\tile ->
                            let
                                others =
                                    tiles |> List.filter (\tile2 -> tile2.id /= tile.id)

                                edges =
                                    [ ( "Top", Matrix.getRow 0 tile.matrix )
                                    , ( "Bottom", Matrix.getRow (size - 1) tile.matrix )
                                    , ( "Left", Matrix.getColumn 0 tile.matrix )
                                    , ( "Right", Matrix.getColumn (size - 1) tile.matrix )
                                    ]
                                        |> List.filterMap
                                            (\( text, result ) ->
                                                result |> Result.toMaybe |> Maybe.map (Tuple.pair text)
                                            )
                                        |> List.map
                                            (Tuple.mapSecond
                                                (\edge ->
                                                    others
                                                        |> List.map (matches edge)
                                                        |> List.filter (\n -> n > 0)
                                                        |> List.length
                                                )
                                            )
                            in
                            ( tile, edges )
                        )
                    |> List.filter
                        (\( _, edges ) ->
                            edges
                                |> List.filter (\( _, n ) -> n == 0)
                                |> List.length
                                |> (\n -> n >= 2)
                        )
                    |> (\list ->
                            let
                                _ =
                                    Debug.log "product" (list |> List.map (Tuple.first >> .id) |> List.product)
                            in
                            list
                       )
                    |> List.map
                        (\( tile, edges ) ->
                            Html.div [ Html.Attributes.style "margin-top" "24px" ]
                                [ Html.div [ Html.Attributes.style "font-weight" "bold" ]
                                    [ Html.text (String.fromInt tile.id) ]
                                , edges
                                    |> List.map (\( text, num ) -> Html.div [] [ Html.text (text ++ ": " ++ String.fromInt num) ])
                                    |> Html.div []
                                ]
                        )
                    |> Html.div []

            Err message ->
                Html.text message
        ]


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

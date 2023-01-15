module Day11 exposing (..)

import Day11Input exposing (input)
import Hexagon exposing (Hex(..))


output : () -> ( String, String )
output () =
    ( input |> parse |> distances |> .fromStart |> toString
    , input |> parse |> distances |> .maxFromStart |> toString
    )


type Direction
    = N
    | NE
    | SE
    | S
    | SW
    | NW


convertDirection : Direction -> Hexagon.Direction
convertDirection direction =
    case direction of
        N ->
            Hexagon.W

        NE ->
            Hexagon.NW

        SE ->
            Hexagon.NE

        S ->
            Hexagon.E

        SW ->
            Hexagon.SE

        NW ->
            Hexagon.SW


type alias Distances =
    { fromStart : Int
    , maxFromStart : Int
    }


distances : List Direction -> Distances
distances directions =
    let
        convertedDirections =
            List.map convertDirection directions

        start =
            IntCubeHex ( 0, 0, 0 )

        step direction ( hexagon, maxFromStart ) =
            let
                next =
                    Hexagon.neighbor hexagon direction

                fromStart =
                    Hexagon.distance start next
            in
            ( next, max maxFromStart fromStart )

        ( end, maxFromStart ) =
            List.foldl step ( start, 0 ) convertedDirections

        fromStart =
            Hexagon.distance start end
    in
    { fromStart = fromStart
    , maxFromStart = maxFromStart
    }


parse : String -> List Direction
parse string =
    string
        |> String.split ","
        |> List.filterMap parseDirection


parseDirection : String -> Maybe Direction
parseDirection string =
    case string of
        "n" ->
            Just N

        "ne" ->
            Just NE

        "se" ->
            Just SE

        "s" ->
            Just S

        "sw" ->
            Just SW

        "nw" ->
            Just NW

        _ ->
            Nothing

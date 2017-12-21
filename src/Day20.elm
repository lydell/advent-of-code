module Day20 exposing (..)

import Day20Input exposing (input)
import Regex exposing (HowMany(AtMost), Match, Regex)


output : () -> ( String, String )
output () =
    -- This gave me three candidates. I simply tried submitting them in order.
    ( parse input |> findSlowest |> toString
    , ""
    )


type alias Particle =
    { x : Int
    , y : Int
    , z : Int
    , vx : Int
    , vy : Int
    , vz : Int
    , ax : Int
    , ay : Int
    , az : Int
    }


parse : String -> List Particle
parse =
    String.lines >> List.filterMap parseLine


tripletRegex : Regex
tripletRegex =
    Regex.regex "[\\d,-]{5,}"


parseLine : String -> Maybe Particle
parseLine string =
    let
        values =
            string
                |> Regex.find (AtMost 3) tripletRegex
                |> List.map (.match >> parseTriplet)
    in
    case values of
        [ Just ( x, y, z ), Just ( vx, vy, vz ), Just ( ax, ay, az ) ] ->
            Just
                { x = x
                , y = y
                , z = z
                , vx = vx
                , vy = vy
                , vz = vz
                , ax = ax
                , ay = ay
                , az = az
                }

        _ ->
            Nothing


parseTriplet : String -> Maybe ( Int, Int, Int )
parseTriplet string =
    let
        values =
            string
                |> String.split ","
                |> List.map String.toInt
    in
    case values of
        [ Ok x, Ok y, Ok z ] ->
            Just ( x, y, z )

        _ ->
            Nothing


findSlowest : List Particle -> List Int
findSlowest particles =
    let
        idsWithAccs =
            particles
                |> List.indexedMap
                    (\i { ax, ay, az } ->
                        ( i, abs ax + abs ay + abs az )
                    )

        minimum =
            idsWithAccs
                |> List.map Tuple.second
                |> List.minimum
    in
    case minimum of
        Just acc ->
            idsWithAccs
                |> List.filter (Tuple.second >> (==) acc)
                |> List.map Tuple.first

        Nothing ->
            []

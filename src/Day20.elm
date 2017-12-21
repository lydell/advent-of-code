module Day20 exposing (..)

import Day20Input exposing (input)
import Regex exposing (HowMany(AtMost), Match, Regex)


output : () -> ( String, String )
output () =
    -- This gave me three candidates. I simply tried submitting them in order.
    ( parse input |> findSlowest |> toString
      -- This prints the same length over and over after a while.
    , parse input |> stepN (truncate 1.0e3) |> toString
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


step : List Particle -> List Particle
step particles =
    List.map stepParticle particles


stepParticle : Particle -> Particle
stepParticle { x, y, z, vx, vy, vz, ax, ay, az } =
    let
        newVx =
            vx + ax

        newVy =
            vy + ay

        newVz =
            vz + az
    in
    { x = x + newVx
    , y = y + newVy
    , z = z + newVz
    , vx = newVx
    , vy = newVy
    , vz = newVz
    , ax = ax
    , ay = ay
    , az = az
    }


stepN : Int -> List Particle -> Int
stepN n particles =
    case particles of
        [] ->
            0

        _ ->
            if n > 0 then
                let
                    _ =
                        Debug.log "#" (List.length particles)
                in
                stepN (n - 1) (collide (step particles))

            else
                List.length particles


collide : List Particle -> List Particle
collide particles =
    case particles of
        [] ->
            []

        first :: rest ->
            let
                firstPos =
                    getPos first

                filtered =
                    rest
                        |> List.filter
                            (\particle ->
                                firstPos /= getPos particle
                            )
            in
            if List.length filtered == List.length rest then
                first :: collide rest

            else
                collide filtered


getPos : Particle -> ( Int, Int, Int )
getPos { x, y, z } =
    ( x, y, z )

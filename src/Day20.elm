module Day20 exposing (..)

import Day20Input exposing (input)
import Regex exposing (HowMany(AtMost), Match, Regex)


output : () -> ( String, String )
output () =
    -- If the same closest particles repeat 1000 times, consider it done.
    ( parse input |> findClosest 1000 |> toString
      -- If the same length repeats 100 times, consider it done.
    , parse input |> findSurvivors 100 |> toString
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


findClosest : Int -> List Particle -> List Int
findClosest threshold particles =
    findClosestHelper threshold particles ( [], 0 )


findClosestHelper : Int -> List Particle -> ( List Int, Int ) -> List Int
findClosestHelper threshold particles ( lastClosest, seenTimes ) =
    case particles of
        [] ->
            []

        _ ->
            if seenTimes > threshold then
                lastClosest

            else
                let
                    newParticles =
                        step particles

                    idsWithDistances =
                        newParticles
                            |> List.indexedMap
                                (\id particle ->
                                    ( id, getDistance particle )
                                )

                    minDistance =
                        idsWithDistances
                            |> List.map Tuple.second
                            |> List.minimum
                            |> Maybe.withDefault 0

                    closest =
                        idsWithDistances
                            |> List.filter (Tuple.second >> (==) minDistance)
                            |> List.map Tuple.first

                    newResult =
                        if closest == lastClosest then
                            ( lastClosest, seenTimes + 1 )

                        else
                            ( closest, 1 )
                in
                findClosestHelper threshold newParticles newResult


findSurvivors : Int -> List Particle -> Int
findSurvivors threshold particles =
    findSurvivorsHelper threshold particles ( 0, 0 )


findSurvivorsHelper : Int -> List Particle -> ( Int, Int ) -> Int
findSurvivorsHelper threshold particles ( lastLength, seenTimes ) =
    case particles of
        [] ->
            0

        _ ->
            if seenTimes > threshold then
                lastLength

            else
                let
                    newParticles =
                        collide (step particles)

                    length =
                        List.length newParticles

                    newResult =
                        if length == lastLength then
                            ( lastLength, seenTimes + 1 )

                        else
                            ( length, 1 )
                in
                findSurvivorsHelper threshold newParticles newResult


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


getDistance : Particle -> Int
getDistance { x, y, z } =
    abs x + abs y + abs z

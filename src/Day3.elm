module Day3 exposing (distance, output)


output : ( String, String )
output =
    ( 325489 |> distance |> toString
    , ""
    )


distance : Int -> Int
distance number =
    let
        ringNumber =
            ceiling (sqrt (toFloat number)) // 2 + 1

        ringSideLength =
            2 * ringNumber - 1

        numNumbersInRing =
            max 1 (4 * (ringSideLength - 1))

        ringStart =
            max 0 (ringSideLength - 2) ^ 2 + 1

        ringEnd =
            ringStart + numNumbersInRing - 1

        numberIndex =
            number - ringStart

        minDistance =
            ringNumber - 1

        maxDistance =
            2 * minDistance

        period =
            max 1 (toFloat maxDistance)

        amplitude =
            toFloat (maxDistance - minDistance) / 2

        xOffset =
            1

        yOffset =
            amplitude + toFloat minDistance

        k =
            2 * pi / period

        distance =
            amplitude * cos (k * (toFloat numberIndex + xOffset)) + yOffset
    in
    round distance

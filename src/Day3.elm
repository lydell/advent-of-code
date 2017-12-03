module Day3 exposing (distance, output, straightCos)


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
            1 / period

        distance =
            amplitude * straightCos (k * (toFloat numberIndex + xOffset)) + yOffset
    in
    round distance


{-| Like `cos`, but with straight lines between tops and bottoms: `/\/\/`
Also, the period is 1 instead of 2π.
-}
straightCos : Float -> Float
straightCos float =
    let
        decimals =
            abs (float - toFloat (truncate float))

        distance =
            abs (0.5 - decimals)
    in
    distance * 4 - 1

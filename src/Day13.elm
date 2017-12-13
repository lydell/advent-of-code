module Day13 exposing (..)


output : () -> ( String, String )
output () =
    ( input |> parse |> totalSeverity |> toString
    , input |> parse |> findUncaughtDelay |> toString
    )


parse : String -> List ( Int, Int )
parse string =
    string
        |> String.lines
        |> List.filterMap parseLine


parseLine : String -> Maybe ( Int, Int )
parseLine string =
    let
        words =
            string
                |> String.filter ((/=) ',')
                |> String.words
    in
    case String.split ": " string of
        [ depthString, rangeString ] ->
            case ( String.toInt depthString, String.toInt rangeString ) of
                ( Ok depth, Ok range ) ->
                    Just ( depth, range )

                _ ->
                    Nothing

        _ ->
            Nothing


getsCaught : Int -> Int -> Bool
getsCaught n range =
    n % ((range - 1) * 2) == 0


severity : ( Int, Int ) -> Int
severity ( depth, range ) =
    if getsCaught depth range then
        depth * range

    else
        0


totalSeverity : List ( Int, Int ) -> Int
totalSeverity =
    List.map severity >> List.sum


findUncaughtDelay : List ( Int, Int ) -> Int
findUncaughtDelay =
    findUncaughtDelayHelper 0


findUncaughtDelayHelper : Int -> List ( Int, Int ) -> Int
findUncaughtDelayHelper delay list =
    let
        caught =
            list
                |> List.any
                    (\( depth, range ) ->
                        getsCaught (depth + delay) range
                    )
    in
    if caught then
        findUncaughtDelayHelper (delay + 1) list

    else
        delay

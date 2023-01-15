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


input : String
input =
    """0: 5
1: 2
2: 3
4: 4
6: 6
8: 4
10: 6
12: 10
14: 6
16: 8
18: 6
20: 9
22: 8
24: 8
26: 8
28: 12
30: 12
32: 8
34: 8
36: 12
38: 14
40: 12
42: 10
44: 14
46: 12
48: 12
50: 24
52: 14
54: 12
56: 12
58: 14
60: 12
62: 14
64: 12
66: 14
68: 14
72: 14
74: 14
80: 14
82: 14
86: 14
90: 18
92: 17"""

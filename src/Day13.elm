module Day13 exposing (..)

import Day13Input exposing (puzzleInput)
import Html exposing (Html)
import Html.Attributes
import LineParser
import List.Extra as List


type BusId
    = X
    | Id Int


parse : String -> Result String ( Int, List BusId )
parse input =
    case String.lines (String.trim input) of
        [ first, second ] ->
            Result.map2 Tuple.pair
                (String.toInt first |> Result.fromMaybe "First line is not a number.")
                (LineParser.parseGeneral "Comma-separated item"
                    identity
                    (\item ->
                        case item of
                            "x" ->
                                Ok X

                            _ ->
                                String.toInt item
                                    |> Result.fromMaybe "Expected 'x' or a number."
                                    |> Result.map Id
                    )
                    (String.split "," second)
                )

        lines ->
            Err
                ("Expected two lines of input but got "
                    ++ String.fromInt (List.length lines)
                    ++ ": "
                    ++ input
                )


solve1 : Int -> List BusId -> Maybe Int
solve1 estimate ids =
    ids
        |> List.filterMap
            (\busId ->
                case busId of
                    X ->
                        Nothing

                    Id id ->
                        Just ( id, ceiling (toFloat estimate / toFloat id) * id )
            )
        |> List.minimumBy Tuple.second
        |> Maybe.map (\( id, minutes ) -> (minutes - estimate) * id)


allArePrimes : String -> Result String Bool
allArePrimes =
    parse >> Result.map (Tuple.second >> allArePrimesHelper)


allArePrimesHelper : List BusId -> Bool
allArePrimesHelper =
    List.filterMap
        (\busId ->
            case busId of
                X ->
                    Nothing

                Id id ->
                    Just id
        )
        >> List.all isPrime


isPrime : Int -> Bool
isPrime n =
    isPrimeHelper n 2 1


isPrimeHelper : Int -> Int -> Int -> Bool
isPrimeHelper n x i =
    if toFloat x > sqrt (toFloat n) then
        True

    else if (n |> modBy x) == 0 then
        False

    else
        isPrimeHelper n (x + i) 2


solve2 iterationsLeft startTimestamp inc foundItems restItems =
    case restItems of
        [] ->
            startTimestamp

        first :: rest ->
            let
                items =
                    first :: foundItems

                timestamp1 =
                    findTimestamp iterationsLeft startTimestamp inc items

                timestamp2 =
                    findTimestamp iterationsLeft (timestamp1 + inc) inc items

                -- _ =
                --     Debug.log "data" { timestamps = ( timestamp1, timestamp2 ), inc = inc, found = List.length foundItems, rest = List.length restItems, startTimestamp = startTimestamp }
                nextInc =
                    timestamp2 - timestamp1
            in
            if timestamp1 == -1 || timestamp2 == -1 || iterationsLeft <= 0 then
                -1

            else
                solve2 (iterationsLeft - 1) timestamp1 nextInc items rest


findTimestamp iterationsLeft startTimestamp inc items =
    let
        isMatch =
            items
                |> List.all
                    (\( index, id ) ->
                        startTimestamp + index |> modBy id |> (==) 0
                    )
    in
    if isMatch then
        startTimestamp

    else if iterationsLeft <= 0 then
        -1

    else
        findTimestamp (iterationsLeft - 1) (startTimestamp + inc) inc items


part2Stuff : List Int
part2Stuff =
    helper 100000 1 []


helper : Int -> Int -> List Int -> List Int
helper iterationsLeft n result =
    if iterationsLeft <= 0 then
        List.reverse result

    else if (643 * n - 31 |> modBy 509) == 0 then
        helper (iterationsLeft - 1) (n + 1) (n :: result)

    else
        helper (iterationsLeft - 1) (n + 1) result


stress () =
    stressHelper 327287 0 100000000000000


stressHelper inc n target =
    if n >= target then
        n

    else
        stressHelper inc (n + inc) target


main : Html Never
main =
    case parse puzzleInput of
        Ok ( estimate, busIds ) ->
            let
                items =
                    busIds
                        |> List.indexedMap Tuple.pair
                        |> List.filterMap
                            (\( index, busId ) ->
                                case busId of
                                    X ->
                                        Nothing

                                    Id id ->
                                        Just ( index, id )
                            )

                product =
                    items |> List.map Tuple.second |> List.product

                largest =
                    items |> List.maximumBy Tuple.second
            in
            Html.div []
                [ showResult (solve1 estimate busIds |> Result.fromMaybe "No solution found.")
                , Html.text (String.fromInt (solve2 100000 0 1 [] items))
                , Html.br [] []
                , List.map2 (-)
                    part2Stuff
                    (0 :: part2Stuff)
                    |> List.map String.fromInt
                    |> String.join ", "
                    |> Html.text

                -- , Html.text (String.fromInt (stress ()))
                , showResult
                    (allArePrimes """
1
1789,37,47,1889
                        """
                        |> Result.map
                            (\b ->
                                if b then
                                    1

                                else
                                    0
                            )
                    )
                , viewRange 0 4000 items
                ]

        Err error ->
            Html.text error


viewRange : Int -> Int -> List ( Int, Int ) -> Html msg
viewRange from to items =
    Html.table []
        (Html.tbody []
            [ Html.tr []
                (Html.th []
                    [ Html.text "t" ]
                    :: (items
                            |> List.map
                                (\( index, id ) ->
                                    Html.th []
                                        [ Html.text ("[" ++ String.fromInt index ++ ":" ++ String.fromInt id ++ "]") ]
                                )
                       )
                )
            ]
            :: (List.range from to
                    |> List.map
                        (\t ->
                            Html.tr []
                                (Html.th []
                                    [ Html.text (String.fromInt t) ]
                                    :: (items
                                            |> List.map
                                                (\( _, id ) ->
                                                    Html.td []
                                                        (if (t |> modBy id) == 0 then
                                                            [ Html.span [ Html.Attributes.title (String.fromInt id) ]
                                                                [ Html.text "D" ]
                                                            ]

                                                         else
                                                            []
                                                        )
                                                )
                                       )
                                )
                        )
               )
        )


showResult : Result String Int -> Html msg
showResult result =
    Html.output []
        [ Html.text
            (case result of
                Ok int ->
                    String.fromInt int

                Err error ->
                    error
            )
        ]


shortInput : String
shortInput =
    """
1
1789,37,47,1889
"""

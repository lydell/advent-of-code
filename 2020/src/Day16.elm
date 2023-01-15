module Day16 exposing (..)

import Array exposing (Array)
import Day16Input exposing (puzzleInput)
import Dict exposing (Dict)
import Html exposing (Html)
import LineParser
import Set exposing (Set)


type alias Input =
    { rules : Dict String (Set Int)
    , yourTicket : List Int
    , nearbyTickets : List (List Int)
    }


parse : String -> Result String Input
parse input =
    let
        parts =
            input |> String.trim |> String.split "\n\n"
    in
    case parts of
        [ first, second, third ] ->
            Result.map3 Input
                (parseRules first)
                (parseTicket (dropFirstLine second))
                (LineParser.parse parseTicket (dropFirstLine third))

        _ ->
            Err ("Expected 3 parts but got " ++ String.fromInt (List.length parts))


dropFirstLine : String -> String
dropFirstLine =
    String.lines
        >> List.drop 1
        >> String.join "\n"


parseRules : String -> Result String (Dict String (Set Int))
parseRules =
    LineParser.parse
        (\line ->
            case String.split ": " line of
                [ before, after ] ->
                    String.split " or " after
                        |> LineParser.parseGeneral "Range"
                            identity
                            (\range ->
                                case String.split "-" range of
                                    [ lower, upper ] ->
                                        Result.map2 List.range
                                            (String.toInt lower |> Result.fromMaybe "Lower is not a number.")
                                            (String.toInt upper |> Result.fromMaybe "Upper is not a number.")

                                    parts ->
                                        Err ("Expected 2 dash-separated parts but got " ++ String.fromInt (List.length parts))
                            )
                        |> Result.map
                            (List.concat
                                >> Set.fromList
                                >> Tuple.pair before
                            )

                parts ->
                    Err ("Expected 2 colon-separated parts but got " ++ String.fromInt (List.length parts))
        )
        >> Result.map Dict.fromList


parseTicket : String -> Result String (List Int)
parseTicket =
    String.split ","
        >> LineParser.parseGeneral "Number"
            identity
            (String.toInt >> Result.fromMaybe "Not a number.")


solution1 : String -> Result String Int
solution1 =
    parse >> Result.map solve1


solve1 : Input -> Int
solve1 input =
    let
        fullRange : Set Int
        fullRange =
            getFullRange input.rules
    in
    input.nearbyTickets
        |> List.concatMap
            (List.filterMap
                (\number ->
                    if Set.member number fullRange then
                        Nothing

                    else
                        Just number
                )
            )
        |> List.sum


getFullRange : Dict k (Set comparable) -> Set comparable
getFullRange =
    Dict.values
        >> List.foldl Set.union Set.empty


solution2 : String -> Result String Int
solution2 =
    parse >> Result.andThen solve2


solve2 : Input -> Result String Int
solve2 input =
    let
        fullRange : Set Int
        fullRange =
            getFullRange input.rules

        validTickets : List (Array Int)
        validTickets =
            input.yourTicket
                :: input.nearbyTickets
                |> List.filter (List.all (\number -> Set.member number fullRange))
                |> List.map Array.fromList

        numbersPerField : List (List Int)
        numbersPerField =
            input.yourTicket
                |> List.indexedMap
                    (\index _ ->
                        validTickets
                            |> List.filterMap (Array.get index)
                    )

        allFields : Set String
        allFields =
            input.rules |> Dict.keys |> Set.fromList

        candidatesPerField : List ( Int, Set String )
        candidatesPerField =
            numbersPerField
                |> List.map
                    (List.map
                        (\number ->
                            input.rules
                                |> Dict.filter (always (Set.member number))
                                |> Dict.keys
                                |> Set.fromList
                        )
                        >> List.foldl Set.intersect allFields
                    )
                |> untilUnchanged removeSinglesFromOthers
                |> List.indexedMap Tuple.pair

        singles : Result String (Dict Int String)
        singles =
            candidatesPerField
                |> LineParser.parseGeneral "Candidate"
                    (\( index, fields ) ->
                        String.fromInt index
                            ++ ": "
                            ++ (fields |> Set.toList |> String.join ", ")
                    )
                    (\( index, fields ) ->
                        case Set.toList fields of
                            [ single ] ->
                                Ok ( index, single )

                            fieldNames ->
                                Err
                                    ("Expected a single field name but got "
                                        ++ String.fromInt (List.length fieldNames)
                                    )
                    )
                |> Result.map Dict.fromList
    in
    singles
        |> Result.map (getAnswer2 input.yourTicket)


getAnswer2 : List Int -> Dict Int String -> Int
getAnswer2 yourTicket singles =
    yourTicket
        |> List.indexedMap
            (\index number ->
                Dict.get index singles
                    |> Maybe.andThen
                        (\field ->
                            if String.startsWith "departure" field then
                                Just number

                            else
                                Nothing
                        )
            )
        |> List.filterMap identity
        |> List.product


untilUnchanged : (a -> a) -> a -> a
untilUnchanged f a =
    let
        nextA =
            f a
    in
    if nextA == a then
        nextA

    else
        untilUnchanged f nextA


removeSinglesFromOthers : List (Set String) -> List (Set String)
removeSinglesFromOthers items =
    case items of
        [] ->
            []

        first :: rest ->
            removeSinglesFromOthersHelper [] first rest


removeSinglesFromOthersHelper : List (Set String) -> Set String -> List (Set String) -> List (Set String)
removeSinglesFromOthersHelper before current after =
    case Set.toList current of
        [ single ] ->
            let
                nextBefore =
                    before |> List.map (Set.remove single)

                nextAfter =
                    after |> List.map (Set.remove single)
            in
            case nextAfter of
                [] ->
                    List.reverse (current :: nextBefore)

                next :: rest ->
                    removeSinglesFromOthersHelper (current :: nextBefore) next rest

        _ ->
            case after of
                [] ->
                    List.reverse (current :: before)

                next :: rest ->
                    removeSinglesFromOthersHelper (current :: before) next rest


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
        , showResult (solution2 puzzleInput)
        ]


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

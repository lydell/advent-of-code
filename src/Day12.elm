module Day12 exposing (..)

import Day12Input exposing (input)
import Dict exposing (Dict)
import Set exposing (Set)


output : () -> ( String, String )
output () =
    ( input |> parse |> findGroup 0 |> Set.size |> toString
    , input |> parse |> findAllGroups |> List.length |> toString
    )


type alias Connection =
    ( Int, List Int )


type alias ConnectionDict =
    Dict Int (List Int)


parse : String -> ConnectionDict
parse =
    parseLowLevel >> connectionDict


parseLowLevel : String -> List Connection
parseLowLevel string =
    string
        |> String.lines
        |> List.filterMap parseLine


parseLine : String -> Maybe Connection
parseLine string =
    let
        words =
            string
                |> String.filter ((/=) ',')
                |> String.words
    in
    case words of
        idString :: pipe :: connectedIdStrings ->
            let
                idResult =
                    idString |> String.toInt

                connectedIds =
                    connectedIdStrings
                        |> List.filterMap (String.toInt >> Result.toMaybe)
            in
            case ( idResult, List.length connectedIds ) of
                ( _, 0 ) ->
                    Nothing

                ( Ok id, _ ) ->
                    Just ( id, connectedIds )

                _ ->
                    Nothing

        _ ->
            Nothing


connectionDict : List Connection -> ConnectionDict
connectionDict connections =
    List.foldl (uncurry Dict.insert) Dict.empty connections


findGroup : Int -> ConnectionDict -> Set Int
findGroup id dict =
    findGroupHelper id dict Set.empty


findGroupHelper : Int -> ConnectionDict -> Set Int -> Set Int
findGroupHelper id dict group =
    case Dict.get id dict of
        Just connectedIds ->
            let
                insert connectedId newGroup =
                    if Set.member connectedId newGroup then
                        newGroup

                    else
                        findGroupHelper connectedId dict newGroup
            in
            List.foldl insert (Set.insert id group) connectedIds

        Nothing ->
            group


findAllGroups : ConnectionDict -> List (Set Int)
findAllGroups dict =
    let
        insert id connectedIds groups =
            if List.any (Set.member id) groups then
                groups

            else
                findGroup id dict :: groups
    in
    Dict.foldl insert [] dict

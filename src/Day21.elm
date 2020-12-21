module Day21 exposing (..)

import Day21Input exposing (puzzleInput)
import Dict exposing (Dict)
import Html exposing (Html)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Set exposing (Set)


type alias Food =
    { ingredients : Set String
    , allergens : Set String
    }


parse : String -> Result String (List Food)
parse =
    String.trim
        >> Parser.run (Parser.Extra.loopLineWise foodParser)
        >> Result.mapError Parser.Extra.deadEndsToString


foodParser : Parser Food
foodParser =
    Parser.succeed Food
        |= ingredientsParser
        |. Parser.Extra.spaces
        |. Parser.symbol "("
        |. Parser.Extra.spaces
        |. Parser.keyword "contains"
        |. Parser.Extra.spaces
        |= allergensParser
        |. Parser.Extra.spaces
        |. Parser.symbol ")"


ingredientsParser : Parser (Set String)
ingredientsParser =
    Parser.getChompedString (Parser.chompWhile ((/=) '('))
        |> Parser.map (String.words >> Set.fromList)


allergensParser : Parser (Set String)
allergensParser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = Parser.Extra.spaces
        , item = Parser.getChompedString (Parser.chompWhile Char.isAlpha)
        , trailing = Parser.Forbidden
        }
        |> Parser.map Set.fromList


solution1 : String -> Result String Int
solution1 =
    parse >> Result.map solve1


solve1 : List Food -> Int
solve1 foods =
    let
        ingredientsPerAllergen : Dict String (Set String)
        ingredientsPerAllergen =
            foods
                |> List.foldl
                    (\food dict ->
                        food.allergens
                            |> Set.foldl
                                (\allergen ->
                                    Dict.update allergen
                                        (Maybe.withDefault food.ingredients
                                            >> Set.intersect food.ingredients
                                            >> Just
                                        )
                                )
                                dict
                    )
                    Dict.empty
                |> Debug.log "ingredientsPerAllergen"

        possiblySolved : List ( String, Set String )
        possiblySolved =
            ingredientsPerAllergen
                |> Dict.toList
                |> untilUnchanged removeSinglesFromOthers
                |> Debug.log "left"

        allPossiblySolvedIngredients : Set String
        allPossiblySolvedIngredients =
            possiblySolved |> List.foldl (Tuple.second >> Set.union) Set.empty |> Debug.log "allPossiblySolvedIngredients"

        allIngredients : List String
        allIngredients =
            foods |> List.concatMap (.ingredients >> Set.toList) |> Debug.log "allIngredients"

        noAllergens : Set String
        noAllergens =
            allPossiblySolvedIngredients
                |> Set.diff (Set.fromList allIngredients)
                |> Debug.log "noAllergens"
    in
    allIngredients
        |> List.filter (\ingredient -> Set.member ingredient noAllergens)
        |> List.length


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


removeSinglesFromOthers : List ( String, Set String ) -> List ( String, Set String )
removeSinglesFromOthers items =
    case items of
        [] ->
            []

        first :: rest ->
            removeSinglesFromOthersHelper [] first rest


removeSinglesFromOthersHelper : List ( String, Set String ) -> ( String, Set String ) -> List ( String, Set String ) -> List ( String, Set String )
removeSinglesFromOthersHelper before current after =
    case current |> Tuple.second |> Set.toList of
        [ single ] ->
            let
                nextBefore =
                    before |> List.map (Tuple.mapSecond (Set.remove single))

                nextAfter =
                    after |> List.map (Tuple.mapSecond (Set.remove single))
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


shortInput : String
shortInput =
    """
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
"""

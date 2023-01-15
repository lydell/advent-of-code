module Day21 exposing (..)

import Day21Input exposing (puzzleInput)
import Dict exposing (Dict)
import Html exposing (Html)
import LineParser
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


solution : String -> Result String ( Int, String )
solution =
    parse >> Result.andThen solve


solve : List Food -> Result String ( Int, String )
solve foods =
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

        possiblySolved : Result String (List ( String, String ))
        possiblySolved =
            ingredientsPerAllergen
                |> Dict.toList
                |> untilUnchanged removeSinglesFromOthers
                |> LineParser.parseGeneral "Allergen"
                    (\( allergen, ingredients ) ->
                        allergen ++ ": " ++ (ingredients |> Set.toList |> String.join ", ")
                    )
                    (\( allergen, ingredients ) ->
                        case ingredients |> Set.toList of
                            [ single ] ->
                                Ok ( allergen, single )

                            items ->
                                Err ("Expected a single ingredient but got " ++ String.fromInt (List.length items))
                    )
    in
    Result.map (solveHelper foods) possiblySolved


solveHelper : List Food -> List ( String, String ) -> ( Int, String )
solveHelper foods solved =
    let
        allIngredients : List String
        allIngredients =
            foods |> List.concatMap (.ingredients >> Set.toList)

        noAllergens : Set String
        noAllergens =
            solved
                |> List.map Tuple.second
                |> Set.fromList
                |> Set.diff (Set.fromList allIngredients)

        numOccurrancesOfIngredientsWithoutAllergens : Int
        numOccurrancesOfIngredientsWithoutAllergens =
            allIngredients
                |> List.filter (\ingredient -> Set.member ingredient noAllergens)
                |> List.length

        canonicalDangerousIngredientsList : String
        canonicalDangerousIngredientsList =
            solved
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
                |> String.join ","
    in
    ( numOccurrancesOfIngredientsWithoutAllergens
    , canonicalDangerousIngredientsList
    )


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
        [ showResult (solution puzzleInput)
        ]


showResult : Result String ( Int, String ) -> Html msg
showResult result =
    Html.div []
        (case result of
            Ok ( int, string ) ->
                [ Html.output []
                    [ Html.text (String.fromInt int) ]
                , Html.output []
                    [ Html.text string ]
                ]

            Err error ->
                [ Html.text error ]
        )

module Day18 exposing (..)

import Day18Input exposing (puzzleInput)
import Html exposing (Html)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Pratt


type Expression
    = Literal Int
    | Addition Expression Expression
    | Multiplication Expression Expression


type alias Precedence =
    { addition : Int
    , multiplication : Int
    }


parse : Precedence -> String -> Result String (List Expression)
parse precedence =
    String.trim
        >> Parser.run (Parser.Extra.loopLineWise (expressionParser precedence))
        >> Result.mapError Parser.Extra.deadEndsToString


expressionParser : Precedence -> Parser Expression
expressionParser precedence =
    Pratt.expression
        { oneOf =
            [ Pratt.literal (Parser.map Literal Parser.int)
            , parenthesizedExpressionParser
            ]
        , andThenOneOf =
            [ Pratt.infixLeft precedence.addition (Parser.symbol "+") Addition
            , Pratt.infixLeft precedence.multiplication (Parser.symbol "*") Multiplication
            ]
        , spaces = Parser.Extra.spaces
        }


parenthesizedExpressionParser : Pratt.Config Expression -> Parser Expression
parenthesizedExpressionParser config =
    Parser.succeed identity
        |. Parser.symbol "("
        |= Pratt.subExpression 0 config
        |. Parser.symbol ")"


solution1 : String -> Result String Int
solution1 =
    parse { addition = 1, multiplication = 1 }
        >> Result.map (List.map evaluate >> List.sum)


solution2 : String -> Result String Int
solution2 =
    parse { addition = 2, multiplication = 1 }
        >> Result.map (List.map evaluate >> List.sum)


evaluate : Expression -> Int
evaluate expression =
    case expression of
        Literal int ->
            int

        Addition a b ->
            evaluate a + evaluate b

        Multiplication a b ->
            evaluate a * evaluate b


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

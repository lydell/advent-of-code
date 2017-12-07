module Day1 exposing (inverseCaptcha1, inverseCaptcha2, output)

import Array
import Day1Input exposing (captcha)


output : () -> ( String, String )
output () =
    ( toString (inverseCaptcha1 captcha)
    , toString (inverseCaptcha2 captcha)
    )


inverseCaptchaHelper : (List Int -> List ( Int, Int )) -> String -> Int
inverseCaptchaHelper toPairs input =
    input
        |> String.toList
        |> List.filterMap (String.fromChar >> String.toInt >> Result.toMaybe)
        |> toPairs
        |> List.filter (uncurry (==))
        |> List.map Tuple.first
        |> List.sum


inverseCaptcha1 : String -> Int
inverseCaptcha1 =
    inverseCaptchaHelper toPairs1


inverseCaptcha2 : String -> Int
inverseCaptcha2 =
    inverseCaptchaHelper toPairs2


toPairs1 : List a -> List ( a, a )
toPairs1 chars =
    case List.head chars of
        Nothing ->
            []

        Just first ->
            toPairs1Helper first chars


toPairs1Helper : a -> List a -> List ( a, a )
toPairs1Helper first chars =
    case chars of
        [] ->
            []

        last :: [] ->
            [ ( last, first ) ]

        one :: two :: rest ->
            ( one, two ) :: toPairs1Helper first (two :: rest)


toPairs2 : List a -> List ( a, a )
toPairs2 chars =
    let
        offset =
            List.length chars // 2
    in
    List.filterMap (toPairs2Helper offset chars) (List.indexedMap (,) chars)


toPairs2Helper : Int -> List a -> ( Int, a ) -> Maybe ( a, a )
toPairs2Helper offset chars ( index, char ) =
    let
        charsArray =
            Array.fromList chars

        pos =
            rem (index + offset) (Array.length charsArray)

        other =
            Array.get pos charsArray
    in
    Maybe.map ((,) char) other

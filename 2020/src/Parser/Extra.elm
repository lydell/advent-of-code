module Parser.Extra exposing (deadEndsToString, loopLineWise, spaces)

import Parser exposing ((|.), (|=), Parser)


{-| Copied from <https://github.com/elm/parser/pull/16>
-}
deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Parser.Problem -> String
problemToString p =
    case p of
        Parser.Expecting s ->
            "expecting '" ++ s ++ "'"

        Parser.ExpectingInt ->
            "expecting int"

        Parser.ExpectingHex ->
            "expecting hex"

        Parser.ExpectingOctal ->
            "expecting octal"

        Parser.ExpectingBinary ->
            "expecting binary"

        Parser.ExpectingFloat ->
            "expecting float"

        Parser.ExpectingNumber ->
            "expecting number"

        Parser.ExpectingVariable ->
            "expecting variable"

        Parser.ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        Parser.ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        Parser.ExpectingEnd ->
            "expecting end"

        Parser.UnexpectedChar ->
            "unexpected char"

        Parser.Problem s ->
            "problem " ++ s

        Parser.BadRepeat ->
            "bad repeat"


loopLineWise : Parser a -> Parser (List a)
loopLineWise parser =
    Parser.succeed identity
        |= Parser.sequence
            { start = ""
            , separator = "\n"
            , end = ""
            , spaces = Parser.succeed ()
            , item = parser
            , trailing = Parser.Optional
            }
        |. Parser.end


spaces : Parser ()
spaces =
    Parser.chompWhile ((==) ' ')

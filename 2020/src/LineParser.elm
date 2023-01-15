module LineParser exposing (parse, parseGeneral)


parse : (String -> Result String a) -> String -> Result String (List a)
parse parser input =
    input
        |> String.trim
        |> String.lines
        |> parseGeneral "Line" identity parser


parseGeneral : String -> (a -> String) -> (a -> Result String b) -> List a -> Result String (List b)
parseGeneral name toString parser input =
    let
        results =
            input
                |> List.indexedMap (\index part -> ( index, part, parser part ))

        errors =
            results
                |> List.filterMap
                    (\( index, part, result ) ->
                        case result of
                            Ok _ ->
                                Nothing

                            Err error ->
                                Just ( index, part, error )
                    )

        oks =
            results
                |> List.filterMap
                    (\( _, _, result ) ->
                        case result of
                            Ok a ->
                                Just a

                            Err _ ->
                                Nothing
                    )
    in
    if List.isEmpty errors then
        Ok oks

    else
        errors
            |> List.map
                (\( index, part, error ) ->
                    name ++ " " ++ String.fromInt (index + 1) ++ ": " ++ error ++ "\n    " ++ toString part
                )
            |> String.join "\n\n"
            |> Err

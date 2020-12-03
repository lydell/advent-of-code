module LineParser exposing (parse)


parse : (String -> Result String a) -> String -> Result String (List a)
parse parser input =
    let
        results =
            input
                |> String.trim
                |> String.lines
                |> List.indexedMap (\index line -> ( index, line, parser line ))

        errors =
            results
                |> List.filterMap
                    (\( index, line, result ) ->
                        case result of
                            Ok _ ->
                                Nothing

                            Err error ->
                                Just ( index, line, error )
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
                (\( index, line, error ) ->
                    "Line " ++ String.fromInt (index + 1) ++ ": " ++ error ++ "\n    " ++ line
                )
            |> String.join "\n\n"
            |> Err

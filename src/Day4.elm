module Day4 exposing (numValid, output, validatePassPhrase1, validatePassPhrase2)

import Day4Input exposing (input)
import Set


output : ( String, String )
output =
    ( input |> numValid validatePassPhrase1 |> toString
    , input |> numValid validatePassPhrase2 |> toString
    )


validatePassPhrase1 : String -> Bool
validatePassPhrase1 passPhrase =
    let
        words =
            String.words passPhrase

        uniqueWords =
            Set.fromList words
    in
    List.length words == Set.size uniqueWords


validatePassPhrase2 : String -> Bool
validatePassPhrase2 passPhrase =
    let
        words =
            String.words passPhrase
                |> List.map (String.toList >> List.sort >> String.fromList)

        uniqueWords =
            Set.fromList words
    in
    List.length words == Set.size uniqueWords


numValid : (String -> Bool) -> String -> Int
numValid isValid string =
    string
        |> String.lines
        |> List.filter isValid
        |> List.length

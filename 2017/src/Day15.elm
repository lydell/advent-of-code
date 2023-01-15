module Day15 exposing (..)


output : () -> ( String, String )
output () =
    ( judge ( generatorA, generatorB ) |> toString
    , judgeFiltered ( generatorAFiltered, generatorBFiltered ) |> toString
    )


type alias Generator =
    { value : Int
    , factor : Int
    , filter : Int -> Bool
    }


next : Generator -> Generator
next { value, factor, filter } =
    let
        newValue =
            rem (value * factor) 2147483647

        newGenerator =
            { value = newValue, factor = factor, filter = filter }
    in
    if filter newValue then
        newGenerator
    else
        next newGenerator


judge : ( Generator, Generator ) -> Int
judge =
    judgeN (truncate 4.0e7) 16


judgeFiltered : ( Generator, Generator ) -> Int
judgeFiltered =
    judgeN (truncate 5.0e6) 16


judgeN : Int -> Int -> ( Generator, Generator ) -> Int
judgeN n numBits ( generatorA, generatorB ) =
    judgeNHelper n numBits ( generatorA, generatorB ) 0


judgeNHelper : Int -> Int -> ( Generator, Generator ) -> Int -> Int
judgeNHelper n numBits ( generatorA, generatorB ) count =
    if n > 0 then
        let
            nextA =
                next generatorA

            nextB =
                next generatorB

            diff =
                if matchesNLowestBits numBits nextA.value nextB.value then
                    1
                else
                    0
        in
        judgeNHelper
            (n - 1)
            numBits
            ( nextA, nextB )
            (count + diff)
    else
        count


matchesNLowestBits : Int -> Int -> Int -> Bool
matchesNLowestBits n a b =
    rem (a - b) (2 ^ n) == 0


generateN : Int -> Generator -> List Int
generateN num generator =
    let
        fold result =
            case result of
                [] ->
                    [ next generator ]

                previous :: rest ->
                    next previous :: result
    in
    List.repeat num ()
        |> List.foldl (always fold) []
        |> List.reverse
        |> List.map .value


generatorA : Generator
generatorA =
    { value = 591
    , factor = 16807
    , filter = always True
    }


generatorAFiltered : Generator
generatorAFiltered =
    { generatorA
        | filter = \value -> rem value 4 == 0
    }


generatorB : Generator
generatorB =
    { value = 393
    , factor = 48271
    , filter = always True
    }


generatorBFiltered : Generator
generatorBFiltered =
    { generatorB
        | filter = \value -> rem value 8 == 0
    }

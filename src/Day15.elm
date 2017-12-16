module Day15 exposing (..)


output : () -> ( String, String )
output () =
    ( judge ( startA, startB ) ( generatorA, generatorB ) |> toString
    , ""
    )


judge : ( Int, Int ) -> ( Int -> Int, Int -> Int ) -> Int
judge =
    judgeN (truncate 4.0e7) 16


judgeN : Int -> Int -> ( Int, Int ) -> ( Int -> Int, Int -> Int ) -> Int
judgeN n numBits ( startA, startB ) ( generatorA, generatorB ) =
    judgeNHelper n numBits ( startA, startB ) ( generatorA, generatorB ) 0


judgeNHelper : Int -> Int -> ( Int, Int ) -> ( Int -> Int, Int -> Int ) -> Int -> Int
judgeNHelper n numBits ( startA, startB ) ( generatorA, generatorB ) count =
    if n > 0 then
        let
            nextA =
                generatorA startA

            nextB =
                generatorB startB

            diff =
                if matchesNLowestBits numBits nextA nextB then
                    1
                else
                    0
        in
        judgeNHelper
            (n - 1)
            numBits
            ( nextA, nextB )
            ( generatorA, generatorB )
            (count + diff)
    else
        count


matchesNLowestBits : Int -> Int -> Int -> Bool
matchesNLowestBits n a b =
    rem (a - b) (2 ^ n) == 0


generateN : Int -> Int -> (Int -> Int) -> List Int
generateN num start generator =
    let
        next result =
            case result of
                [] ->
                    generator start :: result

                previous :: rest ->
                    generator previous :: result
    in
    List.repeat num ()
        |> List.foldl (always next) []
        |> List.reverse


generator : Int -> Int -> Int
generator factor number =
    rem (number * factor) 2147483647


generatorA : Int -> Int
generatorA =
    generator factorA


generatorB : Int -> Int
generatorB =
    generator factorB


factorA : Int
factorA =
    16807


factorB : Int
factorB =
    48271


startA : Int
startA =
    591


startB : Int
startB =
    393

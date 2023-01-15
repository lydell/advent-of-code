{-# LANGUAGE DisambiguateRecordFields, DuplicateRecordFields, OverloadedLabels, NoFieldSelectors, OverloadedRecordDot #-}
import qualified Shared
import qualified Data.Map.Strict as Map
import Data.Function ((&))
import Data.Maybe (mapMaybe)

main :: IO ()
main = interact solve

solve :: String -> String
solve input =
    let
        world = Shared.parse input
    in
    world.map
    & Map.toList
    & mapMaybe (\(coordinate, height) ->
        if height == 0 then
            Shared.findShortestPath coordinate world
            & fmap length
        else
            Nothing
    )
    & minimum
    & show

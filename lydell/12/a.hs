{-# LANGUAGE DisambiguateRecordFields, DuplicateRecordFields, OverloadedLabels, NoFieldSelectors, OverloadedRecordDot #-}
import qualified Shared
import Data.Function ((&))
import Data.Maybe (fromJust)

main :: IO ()
main = interact solve

solve :: String -> String
solve input =
    let
        world = Shared.parse input
    in
    Shared.findShortestPath world.start world
    & fromJust
    & length
    & show

{-# LANGUAGE DisambiguateRecordFields, DuplicateRecordFields, OverloadedLabels, NoFieldSelectors, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Shared (World(..), parse, findShortestPath) where

import qualified Data.Map.Strict as Map
import Data.Char (isSpace)
import Data.List (elemIndex, sortOn)
import Data.Maybe (fromJust, mapMaybe, listToMaybe)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Data.Function ((&))


findShortestPath :: (Int, Int) -> World -> Maybe [(Int, Int)]
findShortestPath start world =
    findPath
        (\_ _ -> 1)
        (getNeighbours world)
        start
        world.end

getNeighbours :: World -> (Int, Int) -> Set.Set (Int, Int)
getNeighbours world (x, y) =
    case Map.lookup (x, y) world.map of
        Just height ->
            [ (x + 1, y)
            , (x - 1, y)
            , (x, y + 1)
            , (x, y - 1)
            ]
                & filter (\coordinate ->
                    case Map.lookup coordinate world.map of
                        Just neighbourHeight ->
                            neighbourHeight <= height + 1
                        Nothing ->
                            False
                )
                & Set.fromList
        Nothing ->
            error ("Coordinate not found: " ++ show (x, y))

data World =
    World
        { map :: Map.Map (Int, Int) Int
        , width :: Int
        , height :: Int
        , start :: (Int, Int)
        , end :: (Int, Int)
        }
    deriving (Show)

parse :: String -> World
parse input =
    let
        theLines = lines $ trim input
        height = length theLines
        width = length $ head theLines
        indexToCoordinate index = (index `mod` width, index `div` width)
        flat = concat theLines
        start = elemIndex 'S' flat
        end = elemIndex 'E' flat
        map = Map.fromList $ zipWith (\index char -> (indexToCoordinate index, getHeight char)) [0..] flat
    in
    case (start, end) of
        (Just start, Just end) ->
            World map width height (indexToCoordinate start) (indexToCoordinate end)
        _ ->
            error "No start or end"

-- https://stackoverflow.com/a/6270337
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


getHeight :: Char -> Int
getHeight char =
    let
        newChar =
            if char == 'S' then
                'a'
            else if char == 'E' then
                'z'
            else
                char
    in
    fromEnum newChar - fromEnum 'a'


-- Everything below copied from:
-- https://github.com/krisajenkins/elm-astar/blob/4a000d8d54ae42598548dadc246f005b2e780fde/src/AStar/Generalised.elm
-- Didn't feel like implementing path finding.


findPath ::
    Ord comparable =>
    (comparable -> comparable -> Float)
    -> (comparable -> Set.Set comparable)
    -> comparable
    -> comparable
    -> Maybe [comparable]
findPath costFn moveFn start end =
    initialModel start
        & astar costFn moveFn end

data Model comparable =
    Model
        { evaluated :: Set.Set comparable
        , openSet :: Set.Set comparable
        , costs :: Map.Map comparable Float
        , cameFrom :: Map.Map comparable comparable
        }

initialModel :: Ord comparable => comparable -> Model comparable
initialModel start =
    Model Set.empty (Set.singleton start) (Map.singleton start 0) Map.empty

cheapestOpen :: Ord comparable => (comparable -> Float) -> Model comparable -> Maybe comparable
cheapestOpen costFn model =
    model.openSet
        & Set.toList
        & mapMaybe
            (\position ->
                case Map.lookup position model.costs of
                    Nothing ->
                        Nothing

                    Just cost ->
                        Just ( position, cost + costFn position )
            )
        & sortOn snd
        & listToMaybe
        & fmap fst

reconstructPath :: Ord comparable => Map.Map comparable comparable -> comparable -> [comparable]
reconstructPath cameFrom goal =
    reconstructPathHelper cameFrom goal []


reconstructPathHelper :: Ord comparable => Map.Map comparable comparable -> comparable -> [comparable] -> [comparable]
reconstructPathHelper cameFrom goal acc =
    case Map.lookup goal cameFrom of
        Nothing ->
            reverse acc

        Just next ->
            reconstructPathHelper cameFrom next (goal:acc)

updateCost :: Ord comparable => comparable -> comparable -> Model comparable -> Model comparable
updateCost current neighbour model =
    let
        newCameFrom =
            Map.insert neighbour current model.cameFrom

        newCosts =
            Map.insert neighbour distanceTo model.costs

        distanceTo =
            reconstructPath newCameFrom neighbour
                & length
                & fromIntegral

        newModel =
            model
                { costs = newCosts
                , cameFrom = newCameFrom
                }
    in
    case Map.lookup neighbour model.costs of
        Nothing ->
            newModel

        Just previousDistance ->
            if distanceTo < previousDistance then
                newModel

            else
                model

astar :: Ord comparable =>
    (comparable -> comparable -> Float)
    -> (comparable -> Set.Set comparable)
    -> comparable
    -> Model comparable
    -> Maybe [comparable]
astar costFn moveFn goal model =
    case cheapestOpen (costFn goal) model of
        Nothing ->
            Nothing

        Just current ->
            if current == goal then
                Just (reconstructPath model.cameFrom goal)

            else
                let
                    modelPopped =
                        model
                            { openSet = Set.delete current model.openSet
                            , evaluated = Set.insert current model.evaluated
                            }

                    neighbours =
                        moveFn current

                    newNeighbours =
                        Set.difference neighbours modelPopped.evaluated

                    modelWithNeighbours =
                        modelPopped
                            { openSet =
                                Set.union modelPopped.openSet
                                    newNeighbours
                            }

                    modelWithCosts =
                        Set.foldl (flip (updateCost current)) modelWithNeighbours newNeighbours
                in
                astar costFn moveFn goal modelWithCosts

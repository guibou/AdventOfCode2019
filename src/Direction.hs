module Direction where

import Utils
import Text.Megaparsec

-- This module helps parsing working with directions on a 2D grid

data Direction = North | East | South | West
  deriving (Show, Enum, Bounded, Eq)

pattern U, D, L, R :: Direction
pattern U = North
pattern D = South
pattern L = East
pattern R = West

{-# COMPLETE U, D, L, R #-}

parseUDLR :: Parser Direction
parseUDLR = choice [
  "U" $> U,
  "L" $> L,
  "D" $> D,
  "R" $> R
  ]

parseNSEW :: Parser Direction
parseNSEW = choice [
  "N" $> North,
  "S" $> South,
  "E" $> East,
  "W" $> West
  ]

nextDirection m (x, y) = case m of
  North -> (x, y + 1)
  South -> (x, y - 1)
  West -> (x - 1, y)
  East -> (x + 1, y)

flattenDirections :: [(Direction, Int)] -> [Direction]
flattenDirections = concatMap flattenDirection

flattenDirection :: (Direction, Int) -> [Direction]
flattenDirection (m, v) = replicate v m

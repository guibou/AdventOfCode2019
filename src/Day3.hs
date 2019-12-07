module Day3 where

import Utils

import Text.Megaparsec
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Direction

-- start 16:40

move :: Parser (Direction, Int)
move = (,) <$> parseUDLR <*> parseNumber

fileContent :: ([(Direction, Int)], [(Direction, Int)])
fileContent = let
  [l0, l1] = Text.lines $(getFile)
  in (unsafeParse (move `sepBy` ",") l0, unsafeParse (move `sepBy` ",") l1)

-- * Generics
walkPath :: [(Direction, Int)] -> [(Int, Int)]
walkPath moves = scanl' (flip nextDirection) (0, 0) (flattenDirections moves)

-- * FIRST problem
day :: _ -> Int
day (path0, path1) =
  let position0 = drop 1 $ walkPath path0
      position1 = drop 1 $ walkPath path1

      intersections = Set.intersection (Set.fromList position0) (Set.fromList position1)

  in minimum (map (manhattan (0, 0)) (Set.toList intersections))

manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

-- first star at 16:56

-- * SECOND problem
day' :: _ -> Int
day' (path0, path1) =
  let position0 = Map.fromList (zip (walkPath path0) [0..])
      position1 = Map.fromList (zip (walkPath path1) [0..])

      intersections = Set.delete (0, 0) (Set.intersection (Map.keysSet position0) (Map.keysSet position1))

      f it = let
        Just steps0 = Map.lookup it position0
        Just steps1 = Map.lookup it position1
        in (steps0 + steps1)
  in minimum (map f (Set.toList intersections))

-- second start: 17:01

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 399
    it "on second star" $ do
      day' fileContent `shouldBe` 15678

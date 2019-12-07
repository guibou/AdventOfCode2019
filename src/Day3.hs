module Day3 where

import Utils

import Text.Megaparsec
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List (findIndex)

-- start 16:40

data Move = U | D | L | R
  deriving (Show)

move :: Parser (Move, Int)
move = (,) <$> choice [
  "U" $> U,
  "L" $> L,
  "D" $> D,
  "R" $> R] <*> parseNumber

fileContent :: ([(Move, Int)], [(Move, Int)])
fileContent = let
  [l0, l1] = Text.lines $(getFile)
  in (unsafeParse (move `sepBy` ",") l0, unsafeParse (move `sepBy` ",") l1)

-- * Generics
walkPath :: [(Move, Int)] -> [(Int, Int)]
walkPath moves = scanl nextMove (0, 0) (flattenMoves moves)

flattenMoves moves = concatMap flattenMove moves

flattenMove (m, v) = replicate v m

nextMove (x, y) m = case m of
  U -> (x, y + 1)
  D -> (x, y - 1)
  L -> (x - 1, y)
  R -> (x + 1, y)


-- * FIRST problem
day :: _ -> _
day (path0, path1) =
  let position0 = drop 1 $ walkPath path0
      position1 = drop 1 $ walkPath path1

      intersections = Set.intersection (Set.fromList position0) (Set.fromList position1)

  in minimum (map (manhattan (0, 0)) (Set.toList intersections))

manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

-- first star at 16:56

-- * SECOND problem
day' :: _ -> _
day' (path0, path1) =
  let position0 = walkPath path0
      position1 = walkPath path1

      intersections = Set.delete (0, 0) (Set.intersection (Set.fromList position0) (Set.fromList position1))

      f it = let
        Just steps0 = findIndex (==it) position0
        Just steps1 = findIndex (==it) position1
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

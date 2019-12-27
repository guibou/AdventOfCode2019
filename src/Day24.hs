module Day24 where

-- start 15:45
-- first star 16:03
-- second star 16:46

import Utils
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.Map as Map

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t = Set.fromList $ do
  (y, line) <- zip [0 :: Int ..] $ Text.lines t
  (x, col) <- zip [0 :: Int ..] $ Text.unpack line

  guard (col == '#')

  pure ((x, y), 0)

-- * Generics
caseOffsetToNumber i = let (y, x) = (i - 1) `divMod` 5
                       in (x, y)

toNumber (x, y) = x + 1 + y * 5

evolveCase :: Int -> (Int, Int) -> (Int, Int) -> [((Int, Int), Int)]
evolveCase depth current c@(x, y)
  -- Going down
  | toNumber c == 13 = case toNumber current of
               8 -> map ((, depth + 1) . caseOffsetToNumber) [1..5]
               12 -> map ((, depth + 1) . caseOffsetToNumber) [1, 6..21]
               14 -> map ((, depth + 1) . caseOffsetToNumber) [5, 10..25]
               18 -> map ((, depth + 1) . caseOffsetToNumber) [21..25]
               _ -> error "WTF is not current"
  | x == -1 = [(caseOffsetToNumber 12, depth - 1)]
  | x == 5 = [(caseOffsetToNumber 14, depth - 1)]
  | y == -1 = [(caseOffsetToNumber 8, depth - 1)]
  | y == 5 = [(caseOffsetToNumber 18, depth - 1)]
  | inWorld (c, depth) = [(c, depth)]
  | otherwise = []

inWorld ((x, y), _) = x >= 0 && x < 5 && y >= 0 && y < 5

stepBugs bugs goRecursive = do
  let
    bugsCount = Map.fromListWith (+) $ do
      ((x, y), depth) <- Set.toList bugs

      let adjList = [
            (x + 1, y),
            (x - 1, y),
            (x, y - 1),
            (x, y + 1)
            ]

      adj <- if goRecursive
             then concatMap (evolveCase depth (x, y)) adjList
             else (,depth) <$> adjList

      pure $ (adj, 1 :: Int)

    fAlive bug = case Map.lookup bug bugsCount of
      Just 1 -> True
      _ -> False

    alive = Set.filter fAlive bugs
    infested = Map.keysSet $ Map.filter (\x -> x == 1 || x == 2) bugsCount

  Set.filter inWorld $ alive <> (infested `Set.difference` bugs)

disp depth bugs = Text.unlines $ do
  y <- [0..4]
  pure $ Text.pack $ do
    x <- [0..4]

    if ((x, y), depth) `Set.member` bugs
      then pure '#'
      else pure '.'

rate bugs = sum $ zipWith (*) powOfTwo $ do
  y <- [0..4]
  x <- [0..4]

  pure $ if ((x, y), 0) `Set.member` bugs then 1 else 0

powOfTwo = go 1
  where
    go x = x : go (x * 2)

lookForFirst = go Set.empty
  where
    go s bugs
      | bugs `Set.member` s = bugs
      | otherwise = go (Set.insert bugs s) (stepBugs bugs False)

-- * FIRST problem
day :: _ -> Int
day bugs = rate (lookForFirst bugs)

-- * SECOND problem
day' bugs nSteps = length $ applyN nSteps (flip stepBugs True) bugs

-- * Tests
ex = parseContent [fmt|\
....#
#..#.
#..##
..#..
#....
|]


test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 2129920
    it "of second star" $ do
      day' ex 10 `shouldBe` 99
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 7543003
    it "on second star" $ do
      day' fileContent 200 `shouldBe` 1975

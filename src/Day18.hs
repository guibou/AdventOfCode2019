module Day18 where

import Utils
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Char (isLower, isUpper, toLower)
import qualified Data.Text as Text
import Direction
import Path

-- start: 15:04
-- pause: 16:51
-- restart: 18:06
-- pause: 18:25
-- restart: 00:10
-- first star: 00:35

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t = splitWorld $ do
  (line, lineNo) <- zip (Text.lines t) [0 :: Int ..]
  (col, colNo) <- zip (Text.unpack line) [0 :: Int ..]

  pure ((colNo, lineNo), col)

splitWorld items = (walls, keys, doors, start)
  where
    walls = HashSet.fromList $ map fst $ filter (\(_, v) -> v == '#') items
    keys = HashMap.fromList $ filter (\(_, v) -> isLower v) items
    doors = HashMap.fromList $ filter (\(_, v) -> isUpper v) items

    Just (start, '@') = find (\(_, v) -> v == '@') items

buildGraph :: HashSet (Int, Int) -> HashMap (Int, Int) Char -> HashMap (Int, Int) Char -> (Int, Int) -> HashMap Char (HashMap Char (Int, HashSet Char, HashSet Char))
buildGraph walls keys doors start = HashMap.fromListWith (HashMap.union) $ do
  ((kPos, k), ks) <- select ((start, '@'): HashMap.toList keys)

  (k'Pos, k') <- ks

  let
    function :: (Int, Int) -> [(Int, (Int, Int))]
    function p = do
        dir <- [North, South, East, West]
        let p' = nextDirection dir p

        guard $ not (p' `HashSet.member` walls)

        pure (1, p')

  case shortestPath function (+) kPos k'Pos of
    Nothing -> []
    Just (weight, path) -> let
      doorsOnPath = HashSet.fromList $ map toLower $ catMaybes (map (\p -> HashMap.lookup p doors) path)
      keysOnPath = HashSet.fromList $ catMaybes (map (\p -> HashMap.lookup p keys) path)
      in [
      (k, HashMap.singleton k' (weight, doorsOnPath, keysOnPath))
      ]

-- * Generics

-- * FIRST problem
-- Huge graph
day (walls, keys, doors, start) = minimum $ map fst $ catMaybes $ do

  let graph = buildGraph walls keys doors start

  (_, k) <- HashMap.toList keys

  let
    f :: (Char, HashSet Char) -> [(Int, (Char, HashSet Char))]
    f (p, availableKeys) = do
      (p', (weight, doorsOnPath, keysOnPath)) <- HashMap.toList $ graph HashMap.! p

      guard $ null (doorsOnPath `HashSet.difference` availableKeys)

      pure (weight, (p', availableKeys `HashSet.union` keysOnPath))

  pure (shortestPath f (+) ('@', HashSet.empty) (k, HashSet.fromList (HashMap.elems keys)))

-- A vertex is named ('x', set(...)).
-- ('x' is the key and the set is the set of owned keys)


-- We'll look for the fasted path (using dijkstra) to forall x (x, fullset)


traceShowId' :: Show a => [Char] -> a -> a
traceShowId' s v = trace (s <> show v) v

-- * SECOND problem
day' :: _ -> Int
day' = undefined

-- * Tests
ex0 = parseContent [fmt|\
#########
#b.A.@.a#
#########
|]

ex1 = parseContent [fmt|\
########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
|]

ex2 = parseContent [fmt|\
########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################
|]

-- 6030 too high for first star
-- 5242 is too high
-- 5006 is too high
-- 4948 is too high

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 8
      day ex1 `shouldBe` 86
      day ex2 `shouldBe` 132
--    it "of second star" $ do
--      day' "" `shouldBe` 0
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 4590
--    it "on second star" $ do
--      day' fileContent `shouldBe` 1238

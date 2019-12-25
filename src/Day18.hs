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

buildGraph :: (HashSet (Int, Int), HashMap (Int, Int) Char, HashMap (Int, Int) Char, ((Int, Int), Char)) -> HashMap Char (HashMap Char (Int, HashSet Char, HashSet Char))
buildGraph (walls,keys,doors,start) = HashMap.fromListWith (HashMap.union) $ do
  ((kPos, k), ks) <- select (start: HashMap.toList keys)

  (k'Pos, k') <- ks
  guard $ k < k'

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
      in
      (k, HashMap.singleton k' (weight, doorsOnPath, keysOnPath)):
      if k' /= snd start
      then [(k', HashMap.singleton k (weight, doorsOnPath, keysOnPath))]
      else []

-- * Generics

-- * FIRST problem
-- Huge graph
day (walls, keys, doors, start) = fst $ unsafeFromJust $ do
  let graph = buildGraph (walls, keys, doors, (start, '@'))

  let
    f :: Maybe (Char, HashSet Char) -> [(Int, Maybe (Char, HashSet Char))]
    f Nothing = []
    f (Just (p, availableKeys)) = do
      (p', (weight, doorsOnPath, keysOnPath)) <- HashMap.toList $ graph HashMap.! p

      guard $ not $ p' `HashSet.member` availableKeys
      guard $ null (doorsOnPath `HashSet.difference` availableKeys)

      let newHash = HashSet.insert p' (availableKeys `HashSet.union` keysOnPath)

      if newHash == HashSet.fromList (HashMap.elems keys)
        then pure (weight, Nothing)
        else pure (weight, Just (p', newHash))

  shortestPath f (+) (Just ('@', HashSet.empty)) Nothing

-- A vertex is named ('x', set(...)).
-- ('x' is the key and the set is the set of owned keys)


-- We'll look for the fasted path (using dijkstra) to forall x (x, fullset)
buildGraphs (walls', keys, doors, (sx, sy)) = HashMap.unions $  do
  let
    starts = [(sx + 1, sy + 1),
              (sx - 1, sy - 1),
              (sx - 1, sy + 1),
              (sx + 1, sy - 1)
             ]
    walls = walls' <> HashSet.fromList [
      (sx, sy),
      (sx + 1, sy),
      (sx - 1, sy),
      (sx, sy + 1),
      (sx, sy - 1)]

  start <- zip starts "0123"

  pure $ buildGraph (walls, keys, doors, start)


traceShowId' :: Show a => [Char] -> a -> a
traceShowId' s v = trace (s <> show v) v

-- * SECOND problem
day' (walls', keys', doors, start) = fst $ unsafeFromJust $ do
  let graph = buildGraphs (walls', keys', doors, start)

  let
    f :: (Char, HashSet Char) -> [(Int, Maybe (Char, HashSet Char))]
    f (p, availableKeys) = do
      (p', (weight, doorsOnPath, keysOnPath)) <- maybe [] HashMap.toList (HashMap.lookup p graph)

      guard $ not $ p' `HashSet.member` availableKeys
      guard $ null (doorsOnPath `HashSet.difference` availableKeys)

      let newHash = HashSet.insert p' (availableKeys `HashSet.union` keysOnPath)

      if newHash == HashSet.fromList (HashMap.elems keys')
        then pure (weight, Nothing)
        else pure (weight, Just (p', newHash))

    f' :: Maybe ((Char, Char, Char, Char), HashSet Char) -> [(Int, Maybe ((Char, Char, Char, Char), HashSet Char))]
    f' Nothing = []
    f' (Just ((a, b, c, d), availableKeys)) =
      let
        as' = f (a, availableKeys)
        bs' = f (b, availableKeys)
        cs' = f (c, availableKeys)
        ds' = f (d, availableKeys)
      in
        (flip map as' $ \case
          (w, Nothing) -> (w, Nothing)
          (w, Just (a', ak')) -> (w, Just ((a', b, c, d), ak'))
        ) <>
        (flip map bs' $ \case
          (w, Nothing) -> (w, Nothing)
          (w, Just (b', bk')) -> (w, Just ((a, b', c, d), bk'))
        ) <>
        (flip map cs' $ \case
          (w, Nothing) -> (w, Nothing)
          (w, Just (c', ck')) -> (w, Just ((a, b, c', d), ck'))
        ) <>
        (flip map ds' $ \case
          (w, Nothing) -> (w, Nothing)
          (w, Just (d', dk')) -> (w, Just ((a, b, c, d'), dk'))
        )


  shortestPath f' (+) (Just (('0', '1', '2', '3'), HashSet.empty)) Nothing

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


ex' = parseContent [fmt|\
#############
#DcBa.#.GhKl#
#.###...#I###
#e#d#.@.#j#k#
###C#...###J#
#fEbA.#.FgHi#
#############
|]

ex'0 = parseContent [fmt|\
#######
#a.#Cd#
##...##
##.@.##
##...##
#cB#Ab#
#######
|]

ex'1 = parseContent [fmt|\
###############
#d.ABC.#.....a#
######...######
######.@.######
######...######
#b.....#.....c#
###############
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
    it "of second star" $ do
      day' ex' `shouldBe` 32
      day' ex'0 `shouldBe` 8
      day' ex'1 `shouldBe` 24
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 4590
    it "on second star" $ do
      day' fileContent `shouldBe` 2086

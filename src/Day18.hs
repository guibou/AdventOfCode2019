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
import Data.List (partition)

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
  guard $ k > k'

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
      ,(k', HashMap.singleton k (weight, doorsOnPath, keysOnPath))
      ]

findLatestKey graph = let
  fromStart = map (\(k, (_, d, _)) -> (k, d)) $ HashMap.toList (graph HashMap.! '@')

  go [(k, _)] = k
  go l = let
    (noDeps, hasDeps) = partition (\(_, d) -> null d) l

    noDepsKey = HashSet.fromList (map fst noDeps)
    hasDeps' = map (\(k, d) -> (k, d `HashSet.difference` noDepsKey)) hasDeps
    in go hasDeps'

  in go fromStart

-- * Generics

-- * FIRST problem
-- Huge graph
day (walls, keys, doors, start) = fst $ unsafeFromJust $ do
  let graph = buildGraph (walls, keys, doors, (start, '@'))

  let k = findLatestKey graph

  let
    f :: (Char, HashSet Char) -> [(Int, (Char, HashSet Char))]
    f (p, availableKeys) = do
      (p', (weight, doorsOnPath, keysOnPath)) <- HashMap.toList $ graph HashMap.! p

      guard $ not $ p' `HashSet.member` availableKeys
      guard $ null (doorsOnPath `HashSet.difference` availableKeys)

      pure (weight, (p', HashSet.insert p' (availableKeys `HashSet.union` keysOnPath)))

  shortestPath f (+) ('@', HashSet.empty) (k, HashSet.fromList (HashMap.elems keys))

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
day' (walls', keys', doors, start) = fst $ minimumBy (comparing fst) $ catMaybes $ do
  let graph = buildGraphs (walls', keys', doors, start)

  -- TODO: optimize this section by just looking for keys that really exists for
  -- each subsection. Because right now we are computing 26 * 26 * 26 * 26 possibilities
  (k0, ks) <- select $ HashMap.elems keys'
  (k1, ks') <- select ks
  (k2, ks'') <- select ks'
  (k3, _) <- select ks''

  let
    f :: (Char, HashSet Char) -> [(Int, (Char, HashSet Char))]
    f (p, availableKeys) = do
      (p', (weight, doorsOnPath, keysOnPath)) <- maybe [] HashMap.toList (HashMap.lookup p graph)

      guard $ not $ p' `HashSet.member` availableKeys
      guard $ null (doorsOnPath `HashSet.difference` availableKeys)

      pure (weight, (p', HashSet.insert p' (availableKeys `HashSet.union` keysOnPath)))

    f' :: ((Char, Char, Char, Char), HashSet Char) -> [(Int, ((Char, Char, Char, Char), HashSet Char))]
    f' ((a, b, c, d), availableKeys) =
      let
        as' = f (a, availableKeys)
        bs' = f (b, availableKeys)
        cs' = f (c, availableKeys)
        ds' = f (d, availableKeys)
      in
        map (\(w, (a', ak')) -> (w, ((a', b, c, d), ak'))) as'
        <>
        map (\(w, (b', bk')) -> (w, ((a, b', c, d), bk'))) bs'
        <>
        map (\(w, (c', ck')) -> (w, ((a, b, c', d), ck'))) cs'
        <>
        map (\(w, (d', dk')) -> (w, ((a, b, c, d'), dk'))) ds'


  pure (shortestPath f' (+) (('0', '1', '2', '3'), HashSet.empty) ((k0, k1, k2, k3), HashSet.fromList (HashMap.elems keys')))

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
--    it "on second star" $ do
--      day' fileContent `shouldBe` 1238

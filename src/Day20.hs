{-# LANGUAGE FlexibleInstances #-}
module Day20 where

import Utils
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import Path

import Data.Char (isLetter)

-- start 18:18. Pause. 18:24. Reprise 18:32
-- first star 19:21...

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t = gridToMaze $ HashMap.fromList $ do
  (line, lineNo) <- zip (Text.lines t) [0..]
  (c, colNo) <- zip (Text.unpack line) [0..]

  pure ((colNo, lineNo), c)


-- * Generics
gridToMaze :: HashMap (Int, Int) Char -> _
gridToMaze input = do
  let
    trueWalls = HashMap.keys $ HashMap.filter (=='#') input
    walls = HashMap.keysSet $ HashMap.filter (/='.') input
    labels = locateLabels input

    start = fst $ unsafeFromJust $ find (\(_, t) -> t == "AA") labels
    end = fst $ unsafeFromJust $ find (\(_, t) -> t == "ZZ") labels

    waypoints = makeWayPoints labels

    minX = (minimum $ map fst $ trueWalls)
    minY = (minimum $ map snd $ trueWalls)
    maxX = (maximum $ map fst $ trueWalls)
    maxY = (maximum $ map snd $ trueWalls)

    bounds = ((minX, minY), (maxX, maxY))

  (walls, waypoints, start, end, bounds)

locateLabels content = go (HashMap.toList onlyLetters)
  where
    onlyLetters = HashMap.filter isLetter content

    go [] = []
    go (((x, y), c):xs)
      | Just oc <- HashMap.lookup (x+1, y) onlyLetters = case HashMap.lookup (x-1, y) content of
          Just '.' -> ((x-1, y), Text.pack [c, oc]) : go xs
          _ -> ((x+2, y), Text.pack [c, oc]) : go xs
      | Just oc <- HashMap.lookup (x, y+1) onlyLetters = case HashMap.lookup (x, y-1) content of
          Just '.' -> ((x, y-1), Text.pack [c, oc]) : go xs
          _ -> ((x, y+2), Text.pack [c, oc]) : go xs
      | otherwise = go xs

makeWayPoints labels = HashMap.fromList $ do
  let
    noStartEnd = filter ((/="AA").snd) . filter ((/="ZZ").snd) $ labels

    mapLabelPositions = HashMap.fromListWith (++) $ do
      (pos, label) <- noStartEnd

      pure (label, [pos])

  (_, [p, p']) <- HashMap.toList $ mapLabelPositions
  [(p, p'), (p', p)]


-- * FIRST problem
day :: _ -> _
day (walls, waypoints, start, end,_) = fst $ unsafeFromJust $ shortestPath f (+) start end
  where
    f :: (Int, Int) -> [(Int, (Int, Int))]
    f p@(x, y) = let
      next = do
        newP <- [
          (x + 1, y),
          (x - 1, y),
          (x, y + 1),
          (x, y - 1)]

        guard $ not $ newP `HashSet.member` walls
        pure (1, newP)
     in maybe identity ((\p' -> ((1, p'):))) (HashMap.lookup p waypoints) next

-- * SECOND problem
day' :: _ -> _
day' (walls, waypoints, start, end, ((minX, minY),(maxX, maxY))) = fst $ unsafeFromJust $ shortestPath f (+) (start, 0) (end, 0)
  where
    isOuter (x, y) = x == minX || x == maxX || y == minY || y == maxY

    f :: ((Int, Int), Int) -> [((Int, ((Int, Int), Int)))]
    f (p@(x, y), level) = let
      next = do
        newP <- [
          (x + 1, y),
          (x - 1, y),
          (x, y + 1),
          (x, y - 1)]

        guard $ not $ newP `HashSet.member` walls
        pure (1, (newP, level))

      currentWaypoint = case HashMap.lookup p waypoints of
        Nothing -> Nothing
        Just p' -> let
          in
           if isOuter p
           then if level > 0
             then Just (p', level - 1)
             else Nothing
           -- WTF: hadoc hack, to limit the depth down the map.
           else if level < 30
              then Just (p', level + 1)
              else Nothing
     in maybe next (\k -> ((1, k):next)) currentWaypoint

-- * Tests
ex = parseContent [fmt|\
         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
DE..#######...###.#
  #.#########.###.#
FG..#########.....#
  ###########.#####
             Z
             Z
             |]

ex' = parseContent [fmt|\
             Z L X W       C
             Z P Q B       K
  ###########.#.#.#.#######.###############
  #...#.......#.#.......#.#.......#.#.#...#
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###
  #.#...#.#.#...#.#.#...#...#...#.#.......#
  #.###.#######.###.###.#.###.###.#.#######
  #...#.......#.#...#...#.............#...#
  #.#########.#######.#.#######.#######.###
  #...#.#    F       R I       Z    #.#.#.#
  #.###.#    D       E C       H    #.#.#.#
  #.#...#                           #...#.#
  #.###.#                           #.###.#
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#
CJ......#                           #.....#
  #######                           #######
  #.#....CK                         #......IC
  #.###.#                           #.###.#
  #.....#                           #...#.#
  ###.###                           #.#.#.#
XF....#.#                         RF..#.#.#
  #####.#                           #######
  #......CJ                       NM..#...#
  ###.#.#                           #.###.#
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#
  #.....#        F   Q       P      #.#.#.#
  ###.###########.###.#######.#########.###
  #.....#...#.....#.......#...#.....#.#...#
  #####.#.###.#######.#######.###.###.#.#.#
  #.......#.......#.#.#.#.#...#...#...#.#.#
  #####.###.#####.#.#.#.#.###.###.#.###.###
  #.......#.....#.#...#...............#...#
  #############.#.#.###.###################
               A O F   N
               A A D   M
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 23
    it "of second star" $ do
      day' ex `shouldBe` 26
      day' ex' `shouldBe` 396
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 514
    it "on second star" $ do
      day' fileContent `shouldBe` 6208

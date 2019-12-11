{-# LANGUAGE MultiWayIf #-}
module Day10 where


import Data.Ratio
import Utils
import qualified Data.Text as Text
import qualified Data.Map as Map

default (Int)

-- start 12:49
-- end 14:13 WTF did I do...
-- second star 14:39

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [(Int, Int)]
parseContent t = do
  (l, y) <- zip (Text.lines t) [0..]
  (a, x) <- zip (Text.unpack l) [0..]

  guard $ a == '#'

  pure ((x, y))

vec (x, y) (x', y') = (x' - x, y' - y)

isColinear (x, y) (x', y') = x * y' - y * x' == 0
dist2 (x, y) (x', y') = sq (x' - x) + sq (y' - y)
  where sq v = v * v

-- * Generics
isOnLine a b c = isColinear ab aTest && max (dist2 a c) (dist2 b c) < dist2 a b
  where
    ab = vec a b
    aTest = vec a c

groto l = do
  (a, l') <- select l

  let friends = do
        (b, l'') <- select l'
        guard $ all (not . isOnLine a b) l''
        pure b

  pure $ (a, friends)

-- * FIRST problem
day :: [(Int, Int)] -> _
day l = maximumBy (comparing snd) $ map length <$> groto l

-- * SECOND problem
day' :: _ -> _ -> Int
day' ms ex = let (a, b) = (mconcat $ transpose $ (fmap (fmap fst)) $ map snd $ boo ms ex) `unsafeIndex` 199
             in a * 100 + b

data Angle = Up | Positive (Ratio Int) | NotUp | Negative (Ratio Int)
  deriving (Show, Eq, Ord)

getAngle (x, y) (x', y') = case compare dx 0 of
  EQ -> if dy < 0 then Up else NotUp
  LT -> Negative (dy % dx)
  GT -> Positive (dy % dx)

  where
    dx = x' - x
    dy = y' - y

groupByAngleAndDistances :: (Int, Int) -> [(Int, Int)] -> Map Angle [((Int, Int), Int)]
groupByAngleAndDistances ms ls = Map.fromListWith (++) $ map f $ (filter (/=ms) ls)
  where
    f x = (getAngle ms x, [(x, dist2 ms x)])

boo ms ls =
  fmap (map (sortBy (comparing snd))) $ sort $ Map.toList $ groupByAngleAndDistances ms ls

-- * Tests

exSmall = parseContent [fmt|\
.#..#
.....
#####
....#
...##|]

displayDetect l = let
  res = groto l
  in display2DGrid (show . length <$> Map.fromList res)

ex0 = parseContent [fmt|\
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####|]

ex1 = parseContent [fmt|\
#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.|]

ex2 = parseContent [fmt|\
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..|]

ex3 = parseContent [fmt|\
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##|]

monitoringStation = (29,28)

test :: Spec
test = do
  -- describe "simple examples" $ do
  --   it "of first star" $ do
  --     day "" `shouldBe` 0
  --   it "of second star" $ do
  --     day' "" `shouldBe` 0
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` (monitoringStation, 256)
    it "on second star" $ do
      day' monitoringStation fileContent `shouldBe` 1707

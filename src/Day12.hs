{-# LANGUAGE DataKinds #-}
module Day12 where

import Utils
import Text.Megaparsec
import Linear
import Data.Generics.Product
import Control.Lens

-- start 13:32
-- first: 13:49
-- pause at 13:56. I have no idea on how to get that fast, so I'm starting day13
-- I'm having a look at that one now that I've finished Day 25 ;)

fileContent :: _
fileContent = parseContent $(getFile)

data Moon = Moon {
  pos :: V3 Integer,
  vel :: V3 Integer
  }
  deriving (Show, Eq, Generic)

parseContent :: Text -> [Moon]
parseContent = unsafeParse $ (`sepBy`"\n") $ do
  void $ "<x="
  x <- parseNumber
  void $ ", y="
  y <- parseNumber
  void $ ", z="
  z <- parseNumber
  void $ ">"
  pure $ Moon (V3 x y z) (V3 0 0 0)

gravity a b = - signum (a - b)

applyGravity (Moon p v) moons = Moon p (foldl' f v moons)
  where
    f acc (Moon pm _) = acc + (gravity <$> p <*> pm)

applyVelicity (Moon p v) = Moon (p + v) v

step moons = moons'
  where
    moonG = map (flip applyGravity moons) moons
    moons' = map applyVelicity moonG

-- * Generics
moonEnergy (Moon (abs -> V3 x y z) (abs -> V3 vx vy vz)) = (x + y + z) * (vx + vy + vz)

-- * FIRST problem
ex0 = parseContent [fmt|\
<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>|]

ex1 = parseContent [fmt|\
<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>|]


day :: [Moon] -> Int -> Integer
day moons n = sum . map moonEnergy . applyN n step $ moons

-- * SECOND problem
-- slow and naive version, does not work
countLoop p start = go start 1
  where
    projStart = map p start

    go (step -> current) !c
      | map p current == projStart = c
      | otherwise = go current (c + 1)

dayAlt :: [Moon] -> _
dayAlt start = let
  counts = do
    axis <- [_x, _y, _z]
    pure $ countLoop (proj axis) start
  in counts

proj axis x = (view (field @"vel" . axis) x, view (field @"pos" . axis) x)


day' = simplify . dayAlt


-- I don't understand what I've found by trial and error
simplify [a, b, c] = (a * b * c) `div` (gcd (gcd (a * b) (b * c)) (a * c))
-- * Tests

--
--
-- 153399385195818 too low

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 10 `shouldBe` 179
      day ex1 100 `shouldBe` 1940
    it "of second star" $ do
      day' ex0 `shouldBe` 2772
      day' ex1 `shouldBe` 4686774924
  describe "works" $ do
    it "on first star" $ do
      day fileContent 1000 `shouldBe` 12773
    it "on second star" $ do
      day' fileContent `shouldBe` 306798770391636

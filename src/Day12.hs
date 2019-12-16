module Day12 where

import Utils
import Text.Megaparsec
import Linear

-- start 13:32
-- first: 13:49
-- pause at 13:56. I have no idea on how to get that fast, so I'm starting day13

fileContent :: _
fileContent = parseContent $(getFile)

data Moon = Moon {
  pos :: V3 Int,
  vel :: V3 Int
  }
  deriving (Show, Eq)

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


day :: [Moon] -> Int -> Int
day moons n = sum . map moonEnergy . applyN n step $ moons

-- * SECOND problem
-- slow and naive version, does not work
day' :: [Moon] -> Int
day' start = go 1 (step start)
  where
    go !count current
      | current == start = count
      | otherwise = go (count + 1) (step current)

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of second star" $ do
      day' ex0 `shouldBe` 2772
  describe "works" $ do
    it "on first star" $ do
      day fileContent 1000 `shouldBe` 12773
    -- it "on second star" $ do
    --   day' fileContent `shouldBe` 1238

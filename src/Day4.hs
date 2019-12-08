module Day4 where

import Utils
import qualified Data.Map.Strict as Map

-- start 17:41

fileContent :: (Int, Int)
fileContent = (356261, 846303)

-- * Generics
checkTwoAdjacent [] = error "WTF2"
checkTwoAdjacent [_] = False
checkTwoAdjacent (x:y:xs)
  | x == y = True
  | otherwise = checkTwoAdjacent (y:xs)

-- first star at 17:53 because I do'nt know how to read

hasAGroupOfOnly2 :: [Int] -> Bool
hasAGroupOfOnly2 digits = length (filter (==2) (Map.elems groups)) >= 1
  where
    groups = Map.fromListWith (+) (map (,1 :: Int) digits)

-- second star 18:00, crap, children are awake ;)

-- 356666 is too high
-- * FIRST problem
compute range p = length $ do
  d0 <- [3 :: Int ..8]
  d1 <- [d0..9]
  d2 <- [d1..9]
  d3 <- [d2..9]
  d4 <- [d3..9]
  d5 <- [d4..9]

  let v = d5 + 10 * (d4 + 10 * (d3 + 10 * (d2 + 10 * (d1 + 10 * d0))))
  guard $ v < snd range && v > fst range

  guard $ p [d0, d1, d2, d3, d4, d5]
  pure ()

day range = compute range checkTwoAdjacent

-- * SECOND problem
day' :: _ -> Int
day' range = compute range hasAGroupOfOnly2

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 544
    it "on second star" $ do
      day' fileContent `shouldBe` 334

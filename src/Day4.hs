module Day4 where

import Utils
import qualified Data.Map.Strict as Map

-- start 17:41

fileContent :: (Int, Int)
fileContent = (356261, 846303)

-- * Generics
toDigits :: Int -> [Int]
toDigits x = map (\d -> ord d - 48) (show x)

isPassword x =
  let digits = toDigits x

  in checkTwoAdjacent digits && checkNonDecreasing digits

isPassword' x =
  let digits = toDigits x

  in hasAGroupOfOnly2 digits && checkNonDecreasing digits

checkTwoAdjacent [] = error "WTF2"
checkTwoAdjacent [_] = False
checkTwoAdjacent (x:y:xs)
  | x == y = True
  | otherwise = checkTwoAdjacent (y:xs)

checkNonDecreasing [] = error "WTF"
checkNonDecreasing [_] = True
checkNonDecreasing (x:y:xs)
  | x <= y = checkNonDecreasing (y:xs)
  | otherwise = False

-- first star at 17:53 because I do'nt know how to read

hasAGroupOfOnly2 :: [Int] -> Bool
hasAGroupOfOnly2 digits = length (filter (==2) (Map.elems groups)) >= 1
  where
    groups = Map.fromListWith (+) (map (,1 :: Int) digits)

-- second star 18:00, crap, children are awake ;)

-- 356666 is too high
-- * FIRST problem
day :: _ -> Int
day range = length $ filter isPassword [fst range..snd range]

-- * SECOND problem
day' :: _ -> Int
day' range = length $ filter isPassword' [fst range..snd range]

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 544
    it "on second star" $ do
      day' fileContent `shouldBe` 334

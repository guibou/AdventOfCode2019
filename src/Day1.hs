module Day1 where

import Utils

-- 15:20

fileContent :: [Int]
fileContent = unsafeParse (some parseNumber) $(getFile)

-- * Generics
fuelForModule mass = mass `div` 3 - 2

-- * FIRST problem
day :: [Int] -> Int
day = sum . map fuelForModule

-- 15:23: first star

crazyFuelRequirement mass
  | mass >= 0 = let
      fuel = fuelForModule mass
      in fuel + (max 0 (crazyFuelRequirement fuel))
  | otherwise = 0

-- * SECOND problem
day' :: [Int] -> Int
day' = sum . map crazyFuelRequirement

-- 15:27 second start

-- * Tests

test :: Spec
test = do
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 3380731
    it "on second star" $ do
      day' fileContent `shouldBe` 5068210

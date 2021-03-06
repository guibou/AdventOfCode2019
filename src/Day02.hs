module Day02 where

import Utils
import Data.Vector hiding (unsafeHead)

import IntCode

-- start 15:30

fileContent :: Vector Int
fileContent = parseIntCode $(getFile)

-- * Generics

-- * FIRST problem
day :: Vector Int -> Int
day v = readIntCodeOutput (v // [(1, 12), (2, 2)])

-- first star: 15:40

-- * SECOND problem
day' :: Vector Int -> Int
day' v = unsafeHead $ do
    noun <- [0..99]
    verb <- [0..99]

    let res = readIntCodeOutput (v // [(1, noun), (2, verb)])

    guard $ res == 19690720
    pure (100 * noun + verb)

-- second start: 15:50
-- I'm an idiot here. The traditional v = v haskell infinite loop killed me

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 4023471
    it "on second star" $ do
      day' fileContent `shouldBe` 8051

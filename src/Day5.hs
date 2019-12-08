module Day5 where

import Utils
import Text.Megaparsec

import IntCode

import Data.Vector as Vector

-- start: 18:03
-- pause: 18:42
-- restart: 18:44
-- pause: 19:09. I'm stuck....
-- restart : 19h37
-- pause: 19:49
-- restart: 21:13
-- first start: 21:34
-- pause before first star

fileContent :: Vector Int
fileContent = Vector.fromList $ unsafeParse (parseNumber `sepBy` ",") $(getFile)

-- * Generics
inputTest = [1]

-- 1935 is too low.

-- * FIRST problem
day :: _ -> Int
day content = Utils.unsafeHead $ snd $ snd $ runState (runIntCode' instructionSet_day5 content) (inputTest, [])


-- * SECOND problem
day' :: _ -> Int
day' = undefined

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 15259545
    it "on second star" $ do
      day' fileContent `shouldBe` 1238

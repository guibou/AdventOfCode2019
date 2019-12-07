module Day2 where

import Utils
import Text.Megaparsec
import Data.Vector hiding (unsafeHead)

-- start 15:30

fileContent :: Vector Int
fileContent = fromList $ unsafeParse (parseNumber `sepBy` ",") $(getFile)

-- * Generics

run :: Vector Int -> Int
run = go 0
  where
    go pos v = case v ! pos of
      1 -> let
        a = v ! (pos + 1)
        b = v ! (pos + 2)
        newVal = v ! a + v ! b
        pos' = v ! (pos + 3)
        in go (pos + 4) (v // [(pos', newVal)])
      2 -> let
        a = v ! (pos + 1)
        b = v ! (pos + 2)
        newVal = v ! a * v ! b
        pos' = v ! (pos + 3)
        in go (pos + 4) (v // [(pos', newVal)])
      99 -> v ! 0

      i -> error $ [fmt|WTF in this computer, case unhandled {i}|]


-- * FIRST problem
day :: Vector Int -> Int
day v = run (v // [(1, 12), (2, 2)])

-- first star: 15:40

-- * SECOND problem
day' :: Vector Int -> Int
day' v = unsafeHead $ do
    noun <- [0..99]
    verb <- [0..99]

    let res = run (v // [(1, noun), (2, verb)])

    guard $ res == 19690720
    pure (100 * noun + verb)

-- second start: 15:50
-- I'm an idiot here. The traditional v = v haskell infinite loop killed me

-- * Tests

test :: Spec
test = do
  describe "woks" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 4023471
    it "on second star" $ do
      day' fileContent `shouldBe` 8051

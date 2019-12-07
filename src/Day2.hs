module Day2 where

import Utils
import Text.Megaparsec
import qualified Data.Vector as V

-- start 15:30

fileContent :: V.Vector Int
fileContent = V.fromList $ (fmap fromInteger <$> unsafeParse (parseNumber `sepBy` ",")) $(getFile)

-- * Generics

run :: V.Vector Int -> Int
run v = go v 0
  where
    go v pos = case (v V.! pos) of
      1 -> let
        a = v V.! (pos + 1)
        b = v V.! (pos + 2)
        newVal = v V.! a + v V.! b
        pos' = v V.! (pos + 3)
        in go (v V.// [(pos', newVal)]) (pos + 4)
      2 -> let
        a = v V.! (pos + 1)
        b = v V.! (pos + 2)
        newVal = v V.! a * v V.! b
        pos' = v V.! (pos + 3)
        in go (v V.// [(pos', newVal)]) (pos + 4)
      99 -> v V.! 0



-- * FIRST problem
day :: V.Vector Int -> Int
day v = run (v V.// [(1, 12), (2, 2)])

-- first star: 15:40

-- * SECOND problem
day' :: V.Vector Int -> Int
day' v = unsafeHead $ do
    noun <- [0..99]
    verb <- [0..99]

    let v' = v V.// [(1, noun), (2, verb)]
        res = run v'

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

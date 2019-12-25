module Day16 where

import Utils hiding (phase)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as VU

-- start: 12:12
-- first: 12:17
-- second: 15:17 (tooks 45 minutes to compute).

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Int]
parseContent = map (\x -> unsafeRead (Text.pack [x])) . Text.unpack

-- * Generics
basePattern = [0 :: Int, 1, 0, -1]

data PatternBlock = PatternBlock
  { startOffset :: !Int
  , mul :: !Int
  } deriving (Show)


pats :: Int -> Int -> [PatternBlock]
pats = pats'

pats' blockLen maxLen = go (blockLen - 1) 1
  where
    go curPos !curMul
      | curPos >= maxLen = []
      | otherwise = PatternBlock curPos curMul : go (curPos + blockLen * 2) (-curMul)

patterns maxLen = map (flip pats maxLen) [1..maxLen]

phase input = VU.generate lenV go
  where
    lenV = VU.length input
    preSum = VU.scanl' (+) 0 input

    go n = (`mod`10) $ abs $ foldl' (+) 0 $ do
      PatternBlock{startOffset, mul} <- pats (n + 1) lenV

      pure $! mul * (preSum VU.! (min lenV (startOffset + n + 1)) - preSum VU.! startOffset)

applyN' :: NFData a => Int -> (a -> a) -> a -> a
applyN' n f s = go 0 s
  where
    go !n' v
      | n == n' = v
      | otherwise = go (n' + 1) $!! (f $!! v)

-- * FIRST problem

day :: [Int] -> Int
day ints = toInt $ VU.toList $ VU.take 8 $ applyN' 100 phase (VU.fromList ints)


-- * SECOND problem
day' :: _ -> _
day' ints = let
  offset = toInt $ take 7 ints

  res = applyN' 100 phase (VU.fromList (mconcat $ replicate 10000 ints))
  in toInt $ VU.toList $ VU.slice offset 8 res

-- Too high: 96324612

ex0' = parseContent "03036732577212944063491565474664"

toInt :: [Int] -> Int
toInt l = unsafeRead $ mconcat $ map (\x -> show x) l

-- * Tests

test :: Spec
test = do
--  describe "simple examples" $ do
--    it "of first star" $ do
--      day "" `shouldBe` 0
--    it "of second star" $ do
--      day' "" `shouldBe` 0
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 29795507
--    it "on second star" $ do
-- FUCKING TOO SLOW!
--      day' fileContent `shouldBe` 89568529

module Day22 where

-- start 16:38
-- first star 17:03... I'm an idiot. I read "what's the card at index XXX" instead of "What is the index of card XXX"

import Utils
import Data.List (findIndex)

import Text.Megaparsec

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [_]
parseContent = unsafeParse $ Text.Megaparsec.many $ choice
  [ dealWithIncrement <$> ("deal with increment " *> parseNumber)
  , cut <$> ("cut " *> parseNumber)
  , dealIntoNewStack <$ "deal into new stack\n"
  ]

-- * Generics
dealIntoNewStack l = reverse l

cut n l
  | n >= 0 = drop n l <> take n l
  | otherwise = drop len l <> take len l
  where len = length l - abs n

dealWithIncrement n l = map snd $ sortBy (comparing fst) $ zipWith f l [0, n..]
  where
    len = length l
    f value idx = (idx `mod` len, value)

-- * FIRST problem
day instructions nbCards idx = unsafeFromJust $ findIndex (==idx) $ (foldl' (.) identity (reverse instructions)) [0 :: Int ..nbCards-1]

-- * SECOND problem
day' :: _ -> Int
day' = undefined

ex = parseContent [fmt|\
deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
|]

ex' = parseContent [fmt|\
deal with increment 7
deal with increment 9
cut -2
|]

ex'' = parseContent [fmt|\
cut 6
deal with increment 7
deal into new stack
|]

ex''' = parseContent [fmt|\
deal with increment 7
deal into new stack
deal into new stack
|]

-- 9399 is too high!
-- 9170 is too high!

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
--     it "of first star" $ do
--       day "" `shouldBe` 0
--     it "of second star" $ do
--       day' "" `shouldBe` 0
   describe "works" $ do
     it "on first star" $ do
       day fileContent 10007 2019 `shouldBe` 6289
--     it "on second star" $ do
--       day' fileContent `shouldBe` 1238

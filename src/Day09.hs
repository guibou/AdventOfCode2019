module Day09 where

import Utils
import IntCode

-- start 14:13
-- first star 14:38
-- first star 14:39

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> Vector Integer
parseContent = parseIntCode

-- * Generics

-- wrong: 235074140399 too high
-- waiting  2350741403

-- * FIRST problem
day :: _ -> Integer
day c = unsafeLast $ runIntCodeOutput instructionSet_day9 c [1]



-- * SECOND problem
day' :: _ -> Integer
day' c = unsafeLast $ runIntCodeOutput instructionSet_day9 c [2]

-- * Tests

ex0 = parseContent "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
ex1 = parseContent "1102,34915192,34915192,7,4,7,99,0"
ex2 = parseContent "104,1125899906842624,99"

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2350741403
    it "on second star" $ do
      day' fileContent `shouldBe` 53088

module Day7 where

import Utils
import Text.Megaparsec
import qualified Data.Vector as V
import IntCode

-- start: 10:47
-- first star: 11:04 (missed the "each phase level can be used only once)
-- pause at 11:09
-- restart 13:17
-- second star 13:22

fileContent :: V.Vector Int
fileContent = Day7.parse $(getFile)

parse = V.fromList . unsafeParse (parseNumber `sepBy` ",")

-- * Generics

readFirstOutput = Utils.unsafeHead . snd . snd

readOutput = snd . snd

-- * FIRST problem
day :: V.Vector Int -> _
day code = maximum $ do
  [a, b, c, d, e] <- permutations [0..4]

  let res = computeThruster code (a, b, c, d, e)
  pure (res, (a, b, c, d, e))

computeThruster code (a, b, c, d, e) =
  let resA = readFirstOutput $ runState (runIntCode' instructionSet_day5' code) ([a, 0], [])
      resB = readFirstOutput $ runState (runIntCode' instructionSet_day5' code) ([b, resA], [])
      resC = readFirstOutput $ runState (runIntCode' instructionSet_day5' code) ([c, resB], [])
      resD = readFirstOutput $ runState (runIntCode' instructionSet_day5' code) ([d, resC], [])
      resE = readFirstOutput $ runState (runIntCode' instructionSet_day5' code) ([e, resD], [])

  in resE

-- 1252067795 is too high

-- * SECOND problem
day' :: V.Vector Int -> _
day' code = maximum $ do
  [a, b, c, d, e] <- permutations [5..9]

  let res = computeThruster' code (a, b, c, d, e)
  pure (res, (a, b, c, d, e))

-- MERCI LA NASA!
computeThruster' code (a, b, c, d, e) =
  let resA = runIntCodeOutput instructionSet_day5' code (a:0:resE)
      resB = runIntCodeOutput instructionSet_day5' code (b:resA)
      resC = runIntCodeOutput instructionSet_day5' code (c:resB)
      resD = runIntCodeOutput instructionSet_day5' code (d:resC)
      resE = runIntCodeOutput instructionSet_day5' code (e:resD)

  in unsafeLast resE

-- * Tests

ex0 = Day7.parse "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
ex1 = Day7.parse "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
ex2 = Day7.parse "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"

-- test :: Spec
-- test = do
--   describe "simple examples" $ do
--     it "of first star" $ do
--       day "" `shouldBe` 0
--     it "of second star" $ do
--       day' "" `shouldBe` 0
--   describe "works" $ do
--     it "on first star" $ do
--       day fileContent `shouldBe` 1228
--     it "on second star" $ do
--       day' fileContent `shouldBe` 1238

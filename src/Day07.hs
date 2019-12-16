module Day07 where

import Utils
import qualified Data.Vector as V
import IntCode

-- start: 10:47
-- first star: 11:04 (missed the "each phase level can be used only once)
-- pause at 11:09
-- restart 13:17
-- second star 13:22

fileContent :: V.Vector Int
fileContent = parseContent $(getFile)

parseContent = parseIntCode

-- * Generics

dayEval feedbackLoop phaseSequence code = maximum $ do
  [a, b, c, d, e] <- permutations phaseSequence

  let res = computeThruster code feedbackLoop (a, b, c, d, e)
  pure (res, (a, b, c, d, e))

-- MERCI LA NASA!
computeThruster code feedbackLoop (a, b, c, d, e) =

  let
    tailInputA = if feedbackLoop then resE else []

    resA = runIntCodeOutput lastInstructionSet code (a:0:tailInputA)
    resB = runIntCodeOutput lastInstructionSet code (b:resA)
    resC = runIntCodeOutput lastInstructionSet code (c:resB)
    resD = runIntCodeOutput lastInstructionSet code (d:resC)
    resE = runIntCodeOutput lastInstructionSet code (e:resD)
  in unsafeLast resE


-- * FIRST problem
day :: V.Vector Int -> _
day = dayEval False [0..4]

-- 1252067795 is too high

-- * SECOND problem
day' :: V.Vector Int -> _
day' = dayEval True [5..9]

-- * Tests

ex0 = parseContent "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
ex1 = parseContent "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
ex2 = parseContent "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"


ex0' = parseContent "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
ex1' = parseContent "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` (43210, (4,3,2,1,0))
      day ex1 `shouldBe` (54321, (0,1,2,3,4))
      day ex2 `shouldBe` (65210, (1,0,4,3,2))
    it "of second star" $ do
      day' ex0' `shouldBe` (139629729, (9,8,7,6,5))
      day' ex1' `shouldBe` (18216, (9,7,8,5,6))
  describe "works" $ do
    it "on first star" $ do
      fst (day fileContent) `shouldBe` 567045
    it "on second star" $ do
      fst (day' fileContent) `shouldBe` 39016654

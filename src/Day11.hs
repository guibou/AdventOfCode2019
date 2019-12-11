module Day11 where

import Utils
import qualified Data.Vector as Vector
import Text.Megaparsec
import qualified Data.Set as Set
import qualified Data.Map as Map


--start 10:16
-- first star 10:38
-- second star 10:42
import IntCode
import Direction

runRobot actions = go (North, (0, 0)) (Set.empty, Set.empty) actions
  where
    go _ (paintedWhite, currentWhite) [] = [Right (paintedWhite, currentWhite)]
    go _ (_, _) [_] = error "WTF one instruction"
    go (curDir, curPos) (paintedWhite, currentWhite) (paint:turn:xs) = let
      (paintedWhite', currentWhite') = case paint of
        0 -> -- paint black
          (paintedWhite, Set.delete curPos currentWhite)
        1 -> -- paint white
          (Set.insert curPos paintedWhite, Set.insert curPos currentWhite)
        _ -> error "WTF painting"

      curDir' = case turn of
        0 -> cyclePred curDir -- turn left
        1 -> cycleSucc curDir -- turn right
        _ -> error "WTF direction"

      curPos' = nextDirection curDir' curPos

      colorCurrent
        | curPos' `Set.member` currentWhite' = 1
        | otherwise = 0

      in Left colorCurrent : go (curDir', curPos') (paintedWhite', currentWhite') xs




fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> Vector Int
parseContent = Vector.fromList . unsafeParse (parseNumber `sepBy` ",")

-- * Generics


-- * FIRST problem
solve :: Int -> Vector Int -> (Set (Int, Int), Set (Int, Int))
solve firstInput content = unsafeHead $ rights algo
  where
    outputs = runIntCodeOutput lastInstructionSet content inputs
    algo = runRobot outputs
    inputs = firstInput:lefts algo

day c = length $ fst $ solve 0 c

-- * SECOND problem
day' content = flipImage $ str2DGrid m
  where
    whites = snd $ solve 1 content
    m = Map.fromSet (const "#") whites

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2255
    it "on second star" $ do
      day' fileContent `shouldBe` [fmt|\
###   ##  #  # #### ###   ##  ###   ##
#  # #  # # #  #    #  # #  # #  # #  #
###  #    ##   ###  #  # #    #  # #  #
#  # #    # #  #    ###  #    ###  ####
#  # #  # # #  #    #    #  # # #  #  #
###   ##  #  # #    #     ##  #  # #  #
|]

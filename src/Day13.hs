module Day13 where

-- start at 13:57. 14:02
-- stop at 14:31. I'm not amazed.
-- restar another day at 17:32
-- pause at 18:01 because children

import Utils

import IntCode
import qualified Data.Map as Map

import qualified Data.Vector as Vector
import System.Console.ANSI

fileContent :: Vector.Vector Int
fileContent = parseIntCode $(getFile)

-- * Generics
-- * FIRST problem
day :: Vector.Vector Int -> Int
day c = go 0 $ runIntCodeOutput lastInstructionSet c []
  where
    go !char [] = char
    go char (_:_:tileMode:xs) = go (case tileMode of
                                      2 -> char + 1
                                      _ -> char) xs
    go _ _ = error "WTF: the input flux of day does not contain groups of 3 values"


runGame :: [Int] -> [(Maybe Int, (_, Int))]
runGame game = go game Map.empty Nothing
  where
    go [] _m _scorem = []
    go (x:y:tileMode:xs) m scorem
      | x == -1 = case scorem of
          Nothing -> (getNewDir m, (m, tileMode)) : go xs m (Just tileMode)
          Just _ -> go xs m (Just tileMode)
      | otherwise =
          let
            m' = Map.insert (x, y) tileMode m
            action = case (scorem, tileMode) of
              (Just score, 4) ->
                let newPos = getNewDir m'
                in ((newPos, (m', score)):)
              _ -> ((Nothing, (m', -1)):)
          in action $ go xs m' scorem
    go _ _ _ = error "WTF: runGame.go input contains invalid values, less than x:y:tileMode"

getNewDir m = case (Map.keys (Map.filter (==4) m),
                     Map.keys (Map.filter (==3) m)) of
                ([oPos], [palPos]) -> Just $ signum (fst (oPos) - fst (palPos))
                _ -> Nothing

dispGame :: _ -> Int -> IO ()
dispGame m score = do
  clearScreen
  disp m
  putText [fmt|Score is {score}\n|]


displayGames [] = putText "end of game"
displayGames ((m, score):xs) = do
  when (score >= 0) $ do
    dispGame m score
    threadDelay (100000)
    -- void $ getLine
  displayGames xs

disp :: Map (Int, Int) Int -> IO ()
disp m = do
  let
    f 0 = " "
    f 1 = "="
    f 2 = "."
    f 3 = "-"
    f 4 = "o"
    f _ = error "WTF disp"

  display2DGrid (map f m)

-- * SECOND problem
day' :: _ -> _
day' c =
  let
    gameOutput = runIntCodeOutput lastInstructionSet (c Vector.// [(0, 2)]) (catMaybes (map fst input))
    input = runGame gameOutput
  in unsafeLast gameOutput

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 296
    it "on second star" $ do
      day' fileContent `shouldBe` 13824

-- star 2: 9472 was too low (I guessed, I was wrong)

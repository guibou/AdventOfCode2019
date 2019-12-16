module Day13 where

-- start at 13:57. 14:02
-- stop at 14:31. I'm not amazed.

import Utils
import Text.Megaparsec

import IntCode
import qualified Data.Map as Map

import qualified Data.Vector as Vector
import System.Console.ANSI

fileContent :: Vector.Vector Int
fileContent = parseContent $(getFile)

parseContent :: Text -> Vector.Vector Int
parseContent = Vector.fromList . unsafeParse (parseNumber `sepBy` ",")

-- * Generics
-- * FIRST problem
day :: Vector.Vector Int -> Int
day c = go 0 $ runIntCodeOutput lastInstructionSet c []
  where
    go !c [] = c
    go c (_:_:tileMode:xs) = go (case tileMode of
                                 2 -> c + 1
                                 _ -> c) xs

displayGame :: [Int] -> IO ()
displayGame game = go game Map.empty
  where
    go :: [Int] -> Map (Int, Int) Int -> IO ()
    go [] m = putText "done\n"
    go (x:y:tileMode:xs) m
      | x == -1 = do
        putText [fmt|Score is {tileMode}\n|]
        go xs m
      | otherwise = do
          let m' = Map.insert (x, y) tileMode m
          clearScreen
          disp m'
          go xs m'

disp :: Map (Int, Int) Int -> IO ()
disp m = do
  putText "------------------"
  let
    f 0 = " "
    f 1 = "="
    f 2 = "."
    f 3 = "-"
    f 4 = "o"

  display2DGrid (map f m)

-- * SECOND problem
day' :: _ -> IO ()
day' c = displayGame game
  where
    input = repeat 0
    game = runIntCodeOutput lastInstructionSet (c Vector.// [(0, 2)]) input

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 296
--    it "on second star" $ do
--      day' fileContent `shouldBe` 1238

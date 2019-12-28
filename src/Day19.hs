module Day19 where

import Utils
import IntCode

-- start 12:27. First 12:36

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = parseIntCode @Int

-- * Generics


-- * FIRST problem
day :: _ -> Int
day code = length $ filter (uncurry (isTracted code)) $ do
  x <- [0 :: Int ..49]
  y <- [0..49]

  pure (x, y)

-- * SECOND problem
isTracted code x y = (unsafeHead $ runIntCodeOutput code [x, y]) == 1

linesBounds :: _ -> _
linesBounds code = (0, 1):(0, 0):go (2 :: Int) (0, 0)
  where
    go !line (a, b) = do
      let
        a' = unsafeFromJust (find (flip (isTracted code) line) [a..])
        bStart = max a' b
        b' = unsafeFromJust (find (not . flip (isTracted code) line) [bStart..])
      (a', b'):go (line + 1) (a', b' - 1)

findBlock :: _ -> Int -> (Int, Int)
findBlock code size = go 0 (linesBounds code)
  where
    go !curLineOffset currentLines
      | Just curColOffset <- checkSize size currentLines = (curLineOffset, curColOffset)
      | otherwise = go (curLineOffset + 1) (drop 1 currentLines)

checkSize size (take size -> l) = let
  a = maximum (map fst l)
  b = minimum (map snd l)
  in if (b - a) >= size
     then Just a
     else Nothing

drawIt code sizeChecked = do
  let (lineOfSizeChecked, _colOfSizeChecked) = findBlock code sizeChecked
  flip mapM_ (take 50 (zip (linesBounds code) [0 :: Int ..])) $ \((startX, endX), i) -> do
    putStrLn $ (if i /= lineOfSizeChecked then [fmt|{i:03}.|] else "--->") <> replicate (startX - 1) '.' <> replicate (endX - startX) '#'

day' :: _ -> Int
day' code =
  let (y, x) = findBlock code 100
  in x * 10000 + y

-- 7261018 is too low!

-- * Tests

test :: Spec
test = do
   describe "works" $ do
     it "on first star" $ do
       day fileContent `shouldBe` 199
     it "on second star" $ do
       day' fileContent `shouldBe` 10180726

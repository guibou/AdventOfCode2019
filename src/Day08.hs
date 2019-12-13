module Day08 where

import Utils
import qualified Data.Text as Text

-- start 13:44
-- first 13:49
-- seconde: 13:58

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Text]
parseContent = Text.chunksOf (imageSizeX * imageSizeY)

imageSizeX = 25
imageSizeY = 6

-- * Generics


-- * FIRST problem
day :: [Text] -> Int
day layers = let
  layer = minimumBy (comparing (Text.count "0")) layers
  in Text.count "1" layer * Text.count "2" layer

-- * SECOND problem
day' :: [Text] -> Text
day' (transpose . (map Text.unpack) -> layers) =
  Text.unlines $ (flip map) [0..(imageSizeY - 1)] $ \y ->
    -- stripEnd is here to make the test happy because my editor is
    -- removing trailing whitespaces ;)
    Text.stripEnd $ Text.pack $ (flip map) [0..(imageSizeX - 1)] $ \x ->
      case find (/= '2') (layers `unsafeIndex` (x + y * imageSizeX)) of
        Just '0' -> ' '
        Just '1' -> '*'

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2080
    it "on second star" $ do
      day' fileContent `shouldBe` [fmt|\
 **  *  * ***   **  *   *
*  * *  * *  * *  * *   *
*  * *  * *  * *     * *
**** *  * ***  *      *
*  * *  * * *  *  *   *
*  *  **  *  *  **    *
|]

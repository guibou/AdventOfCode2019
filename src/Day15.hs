module Day15 where

import Utils
import IntCode
import Direction

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

-- start 10:50. 11:53 first star. 11:55 second star

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = parseIntCode @Int

-- * Generics
directionToInput = \case
  North -> 1
  South -> 2
  West -> 3
  East -> 4

-- * FIRST problem
data WorldStatus = Wall | Free | O2
  deriving (Show, Eq)

directionOpposite North = South
directionOpposite East = West
directionOpposite South = North
directionOpposite West = East

mapWorld :: V.Vector Int -> [Direction] -> [(Direction, Map (Int, Int) WorldStatus)]
mapWorld code input = go machine (0, 0) (Map.singleton (0, 0) Free) []
  where
    go :: [Int] -> (Int, Int) -> Map (Int, Int) WorldStatus -> [Direction] -> [(Direction, Map (Int, Int) WorldStatus)]
    go m pos knownWorld curPath
      | Nothing <- Map.lookup (nextDirection North pos) knownWorld = (North, knownWorld) : readInfo North m pos knownWorld curPath
      | Nothing <- Map.lookup (nextDirection South pos) knownWorld = (South, knownWorld) : readInfo South m pos knownWorld curPath
      | Nothing <- Map.lookup (nextDirection East pos) knownWorld  = (East, knownWorld) : readInfo East m pos knownWorld curPath
      | Nothing <- Map.lookup (nextDirection West pos) knownWorld  = (West, knownWorld) : readInfo West m pos knownWorld curPath

      -- Everything around is is already visited, we backtrack
      | otherwise = case curPath of
          [] -> []
          (lastMove:xs) -> (directionOpposite lastMove, knownWorld) : go (drop 1 m) (nextDirection (directionOpposite lastMove) pos) knownWorld xs

    -- hit a wall
    readInfo curDir (0: m) pos knownWorld curPath = go m pos (Map.insert (nextDirection curDir pos) Wall knownWorld) curPath
    -- right direction
    readInfo curDir (1: m) pos knownWorld curPath = go m (nextDirection curDir pos) (Map.insert (nextDirection curDir pos) Free knownWorld) (curDir:curPath)
    -- found O2
    readInfo curDir (2: m) pos knownWorld curPath = go m (nextDirection curDir pos) (Map.insert (nextDirection curDir pos) O2 knownWorld) (curDir:curPath)

    machine = runIntCodeOutput lastInstructionSet code (map directionToInput input)


drawWorld code = do
  let
    boo = mapWorld code (map fst boo)

    f (0, 0) _ = "S"
    f _ Free = "."
    f _ Wall = "*"
    f _ O2 = "O"

  display2DGrid (Map.mapWithKey f (unsafeLast (map snd boo)))

getWorldMap code = let
  boo = mapWorld code (map fst boo)
  lastGrid = unsafeLast (map snd boo)
  in lastGrid

stepWorld lastGrid pos = do
  dir <- [North, South, East, West]

  let status = Map.lookup (nextDirection dir pos) lastGrid
  guard $ status /= Just Wall

  pure (nextDirection dir pos)

getO2Pos grid = case Map.toList $ Map.filter (==O2) grid of
  [(posO2, O2)] -> posO2
  _ -> error "WTF no O2 in this world?"

day code =
  let
  lastGrid = getWorldMap code

  posO2 = getO2Pos lastGrid
  stopCrit _todos visited _depth = posO2 `Set.member` visited

  (_, _, steps) = bfs stopCrit (0, 0) (stepWorld lastGrid)
  in steps - 1

-- * SECOND problem
day' code = let
  lastGrid = getWorldMap code
  posO2 = getO2Pos lastGrid

  stopCrit todos _visited _depth = null todos

  (_, _, steps) = bfs stopCrit posO2 (stepWorld lastGrid)
  in steps - 1

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 318
    it "on second star" $ do
      day' fileContent `shouldBe` 390

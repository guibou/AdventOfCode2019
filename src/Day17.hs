module Day17 where

import Utils
import Direction

-- start 08:22
-- star at 08:36 (WHAT!!)
-- pause at 08:55
import IntCode
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.Vector as V

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = parseIntCode @Int

-- * Generics
makeGrid output = let
  grid = Text.pack $ map chr output

  in grid

makeGridSet :: Text -> (Set (Int, Int), (Int, Int))
makeGridSet grid =
  let
    gridMap = do
      (line, lineNo) <- zip (Text.splitOn "\n" grid) [0..]
      (col, colNo) <- zip (Text.unpack line) [0..]

      pure ((colNo, lineNo), col)

    gridSet = Set.fromList $ do
      (pos, item) <- gridMap
      guard $ item == '#'

      pure pos

    robotPos = fst $ unsafeFromJust $ find (\(_, c) -> c == '^') gridMap

  in (gridSet, robotPos)

-- * FIRST problem
day :: _ -> _
day code = let
  output = runIntCodeOutput code []
  grid = makeGrid output
  (gridSet, _) = makeGridSet grid

  countNumberIntersections = sum $ do
    let
      maxX = maximum $ map fst (Set.toList gridSet)
      maxY = maximum $ map snd (Set.toList gridSet)

    lineNo <- [0..maxY]
    colNo <- [0..maxX]

    guard $ and $ do
      d <- [North, East, West, South]
      let p' = nextDirection d (colNo, lineNo)

      pure $ p' `Set.member` gridSet

    pure $ lineNo * colNo

  in countNumberIntersections

-- * SECOND problem
day' :: _ -> _
day' code = let
  output = runIntCodeOutput (code V.// [(0, 1)]) []

  grid = makeGrid output
  (gridSet, robotPos) = makeGridSet grid
  path = walkPath robotPos gridSet
  routines = unsafeHead $ breakInRoutine path

  counts = countRoutine routines path

  instructions = formatRoutine counts <> formatMotions routines <> [ord 'n', ord '\n']

  robot = runIntCodeOutput (code V.// [(0, 2)]) instructions

  result = drop (length output) robot

  in unsafeLast result

nextDirectionReverse m (x, y) = case m of
  North -> (x, y - 1)
  South -> (x, y + 1)
  West -> (x - 1, y)
  East -> (x + 1, y)

walkPath start path = compactPath $ go start North
  where
    go curPos curDir
      -- try to continue in this direction
      | let np = nextDirectionReverse curDir curPos
              , np `Set.member` path = Move 1 : go np curDir
      | let np = nextDirectionReverse nd curPos
            nd = succWrap curDir
              , np `Set.member` path = TurnR : Move 1 : go np nd
      | let np = nextDirectionReverse nd curPos
            nd = predWrap curDir
              , np `Set.member` path = TurnL : Move 1 : go np nd
      | otherwise = []

compactPath (Move x : Move x' : xs) = compactPath (Move (x + x') : xs)
compactPath (x : xs) = x : compactPath xs
compactPath [] = []

len20 = filter (\x -> length x <= 20)

breakInRoutine path = do
  a <- drop 1 (len20 (inits path))
  let path' = consumeMany [a] path
  b <- drop 1 (len20 (inits path'))
  let path'' = consumeMany [a,b] path'
  c <- drop 1 (len20 (inits path''))
  let path''' = consumeMany [a,b,c] path''

  guard $ null path'''

  pure (a, b, c)

consumeMany cases = go (sortBy (comparing length) cases)
  where
    go _ [] = []
    go [] p = p

    go (x:xs) p
      | x `isPrefixOf` p = consumeMany cases (drop (length x) p)
      | otherwise = go xs p

data Routines = A | B | C
  deriving (Show, Eq)

countRoutine (a, b, c) = go
  where
    go [] = []
    go p
      | a `isPrefixOf` p = A : go (drop (length a) p)
      | b `isPrefixOf` p = B : go (drop (length b) p)
      | c `isPrefixOf` p = C : go (drop (length c) p)
      | otherwise = error "WTF countRoutine"

formatRoutine routines = map ord $ Text.unpack $ (Text.intercalate "," $ map (Text.pack . show) routines) <> "\n"

formatMotions (a, b, c) = formatMotion a <> formatMotion b <> formatMotion c

formatMotion :: [Move] -> [Int]
formatMotion motions = map ord $ ((intercalate "," $ map f motions) ++ "\n")
  where
    f TurnL = "L"
    f TurnR = "R"
    f (Move i) = show i

data Move = TurnL | TurnR | Move Int
  deriving (Show, Eq)

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 3292
    it "on second star" $ do
      day' fileContent `shouldBe` 651043

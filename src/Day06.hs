module Day06 where

import Utils
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

-- start: 10:17

newtype Object = Object Text
  deriving (Show, Ord, Eq)

fileContent :: [(Object, Object)]
fileContent = parseContent $(getFile)

parseContent c = do
  l <- Text.lines c

  let (item0, Text.drop 1 -> item1) = Text.breakOn ")" l

  pure (Object item0, Object item1)

-- * Generics
countOrbits orbits = countIndirectOrbits orbits

countIndirectOrbits :: [(Object, Object)] -> Int
countIndirectOrbits orbitList = go (Map.elems fullOrbitMap)
  where
    fullOrbitMap :: Map Object Object
    fullOrbitMap = Map.fromList (map swap orbitList)
    go [] = 0
    go l = length l + go (catMaybes $ do
                            o <- l
                            pure $ Map.lookup o fullOrbitMap)

-- * FIRST problem
day :: _ -> Int
day = countOrbits

-- first star : 10:32

-- * SECOND problem
day' :: [(Object, Object)] -> Int
day' orbitList = let
    fullOrbitMap :: Map Object Object
    fullOrbitMap = Map.fromList (map swap orbitList)

    Just myObject = Map.lookup (Object "YOU") fullOrbitMap
    Just santaObject = Map.lookup (Object "SAN") fullOrbitMap

    orbitMapBothWays :: Map Object [Object]
    orbitMapBothWays = Map.fromListWith (++) (map (\(x, y) -> (x, [y])) (Map.toList fullOrbitMap) ++ map (\(x, y) -> (y, [x])) (Map.toList (fullOrbitMap)))

    next p = case Map.lookup p orbitMapBothWays of
      Nothing -> []
      Just v -> v

    (_, _, res) = bfs (\_ visited _ -> santaObject `Set.member` visited) myObject next

    -- This bfs functions returns a depth + 1... I don't remember why ;)
    in res - 1


-- second star: 10:42

-- * Tests

ex0 = parseContent [fmt|\
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L|]

ex2 = parseContent [fmt|\
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 42
    it "of second star" $ do
      day' ex2 `shouldBe` 4
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 162439
    it "on second star" $ do
      day' fileContent `shouldBe` 367

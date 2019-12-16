module Day14 where

import Utils
import Text.Megaparsec

import qualified Data.Map as Map
import qualified Data.Text as Text

-- start at 14:31. first star 15:23. Second star 15:47

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> Map Text (Int, [(Int, Text)])
parseContent t = let
  reactions = unsafeParse (parseReaction `sepBy` "\n") t

  f (ingredients, (n, result)) = (result, (n, ingredients))

  in Map.fromList (map f reactions)

parseReaction = do
  ingredients <- parseItem `sepBy1` ", "
  void " => "
  result <- parseItem

  pure (ingredients, result)

parseItem = do
  n <- parseNumber
  name <- Text.pack <$> Utils.some (oneOf ['A'..'Z'])

  pure (n, name)

-- * Generics
solve :: Map Text (Int, [(Int, Text)]) -> Map Text Int -> Map Text Int
solve rules = go
  where
    go :: Map Text Int -> Map Text Int
    go toProduce = case filter (\(_, n) -> n > 0) (Map.toList toProduce) of
      [("ORE", _)] -> Map.filter (/=0) $ toProduce
      _ -> case find (\(name, c) -> name /= "ORE" && c > 0) $ Map.toList toProduce of
        Nothing -> error "NOTHING TO PRODUCE"
        Just (item, neededToProduce) -> case Map.lookup item rules of
          Nothing -> error ("WTF:" <> item)
          Just (produced, ingrediants) -> let
            nbRound = max 1 (neededToProduce `div` produced)
            in go $ (Map.unionWith (+) toProduce (Map.singleton item (-produced * nbRound) <> (Map.fromList (fmap (*nbRound) <$> (map swap ingrediants)))))

oneFuel = Map.singleton "FUEL" 1

-- * FIRST problem
day :: _ -> Int
day c = solve c oneFuel Map.! "ORE"

-- * SECOND problem
day' :: _ -> Int
day' c = fst $ bisect p (nbEstimatedWithOneTrillion, nbEstimatedWithOneTrillion*2)
  where
    costFuelEstimated = solve c oneFuel Map.! "ORE"

    one_trillion = 10 ^ (12 :: Int)
    nbEstimatedWithOneTrillion = one_trillion `div` costFuelEstimated

    p mid = solve c (Map.singleton "FUEL" mid) Map.! "ORE" < one_trillion

-- Too high: 6216590

-- * Tests
ex0 = parseContent [fmt|\
10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL|]

ex1 = parseContent [fmt|\
9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL|]

ex3Big = parseContent [fmt|\
171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX|]


test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 31
      day ex1 `shouldBe` 165
      day ex3Big `shouldBe` 2210736
    it "of second star" $ do
      day' ex3Big `shouldBe` 460664
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 301997
    it "on second star" $ do
      day' fileContent `shouldBe` 6216589

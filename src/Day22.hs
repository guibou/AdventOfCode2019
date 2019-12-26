{-# LANGUAGE FlexibleInstances #-}
module Day22 where

-- start 16:38
-- first star 17:03... I'm an idiot. I read "what's the card at index XXX" instead of "What is the index of card XXX"
-- second star: 20:37. Well, I had to get my modulo arithmetic back

import Utils hiding ((:*:), (:+:))
import Data.List (findIndex)
import Bezout

import Text.Megaparsec

fileContent :: Draw t => [t]
fileContent = parseContent $(getFile)

parseContent :: (Draw t) => Text -> [t]
parseContent = unsafeParse $ Text.Megaparsec.many $ choice
  [ dealWithIncrement <$> ("deal with increment " *> parseNumber)
  , cut <$> ("cut " *> parseNumber)
  , dealIntoNewStack <$ "deal into new stack\n"
  ]

-- * Generics

-- * FIRST problem
class Draw t where
  dealIntoNewStack :: t
  dealWithIncrement :: Integer -> t
  cut :: Integer -> t

instance Draw ([Integer] -> [Integer]) where
  dealIntoNewStack l = reverse l

  cut n l
    | n >= 0 = drop (fromIntegral n) l <> take (fromIntegral n) l
    | otherwise = drop len l <> take len l
    where len = length l - abs (fromIntegral n)

  dealWithIncrement n l = map snd $ sortBy (comparing fst) $ zipWith f l [0, (fromIntegral n)..]
    where
      len = length l
      f value idx = (idx `mod` len, value)

day :: _ -> Integer -> Integer -> Int
day instructions nbCards idx = unsafeFromJust $ findIndex (==idx) $ (foldl' (.) identity (reverse instructions)) [0 :: Integer ..nbCards-1]

-- * SECOND problem
data Arith
  = Add [Arith]
  | Mul [Arith]
  | Lit Integer
  | Negate Arith
  | Arith :%: Arith
  | Var
  deriving (Show, Eq)

simplify :: Arith -> Arith
simplify = \case
  Negate (Negate v) -> simplify v
  Negate (Mul (x:xs)) -> Mul (map simplify (Negate x: xs))
  Negate (Mul []) -> Lit 0
  l@(Lit _) -> l
  Negate (Add l) -> Add (map (simplify . Negate) l)
  Negate (Lit a) -> Lit (-a)
  Mul ((Add l):xs) -> Add (map (\x -> Mul (map simplify (x:xs))) l)
  Add [x] -> x
  Mul [x] -> x
  Mul x -> Mul (map simplify $ accumLitMul $ concatMap unPackMul x)
  Add l -> Add $ map simplify $ accumLitAdd $ concatMap unPackAdd l
  a :%: (Lit v) -> (simplify $ killMod v a) :%: (Lit v)
  e@(Negate Var) -> e
  Negate e -> Negate $ simplify e
  Var -> Var
  o -> o

killMod :: Integer -> Arith -> Arith
killMod v = \case
  Lit i -> Lit (i `mod` v)
  Negate e -> Negate $ killMod v e
  Mul l -> Mul (map (killMod v) l)
  Var -> Var
  Add l -> Add (map (killMod v) l)
  a :%: (Lit v')
    | v == v' -> killMod v a
    | otherwise -> error "MODULO ARE NOT THE SAME"
  a :%: e -> a :%: e

eval var = \case
  Lit i -> i
  Negate e -> - eval var e
  Mul l -> product (map (eval var) l)
  Add l -> sum (map (eval var) l)
  Var -> var
  a :%: e -> (eval var a) `mod` (eval var e)

simplify' x = let
  x' = simplify x
  in
  if x == x'
  then x'
  else simplify' x'

unPackAdd (Add l) = l
unPackAdd x = [x]

unPackMul (Mul l) = l
unPackMul x = [x]

accumLitAdd l = go 0 l
  where
    go n []
      | n == 0 = []
      | otherwise = [Lit n]
    go n (Lit x: xs) = go (n + x) xs
    go n (x: xs) = x: go n xs

accumLitMul l = go 1 l
  where
    go n []
      | n == 1 = []
      | otherwise = [Lit n]
    go n (Lit x: xs) = go (n * x) xs
    go n (x: xs) = x: go n xs

instance Draw (Integer -> Arith  -> Arith) where
  dealIntoNewStack lenStack posFinal = fromIntegral lenStack - posFinal - 1

  cut n lenStack posFinal = (posFinal + fromIntegral n) `mod` (fromIntegral lenStack)

  dealWithIncrement n lenStack posFinal = let
    invN = inverseMod n lenStack
    in posFinal * (fromIntegral invN) `mod` (fromIntegral lenStack)


infixl 6 `Add`
infixl 7 `Mul`
infixl 7 :%:

instance Num Arith where
  (+) a b = Add [a, b]
  (*) a b = Mul [a, b]
  fromInteger i = Lit i
  negate = Negate
  abs = error "abs"
  signum = error "signum"

instance Ord Arith where
  compare = error "compare"
instance Real Arith where
  toRational = error "toRational"
instance Integral Arith where
  toInteger = error "toInteger"
  mod = (:%:)
  quotRem = error "quotRem"
instance Enum Arith where
  toEnum = error "toEnum"
  fromEnum = error "fromEnum"

foo' :: _ -> [_] -> Arith
foo' deckSize problem = foldl' (.) identity (map ($ deckSize) problem) Var

foo :: [_] -> Arith
foo problem = foldl' (.) identity (map ($ (10 :: Integer)) problem) Var

{-
Add [Mul [Negate Var,Lit 567],Lit 539


(-x) * 567 + 539

-}

{-

We know that:

posFinal = (posCurrent * incr) `mod` len


solution: posCurrent = posFinal * incr^-1 [len]

incr^-1 --> incr * incr^-1 [len] = 1


It means that it exists 'k' such that

posCurrent * incr = k * len + posFinal

===>

posCurrent = (k * len) / incr + posFinal / incr

len is prime (or coprime of incr). So incr cannot divid it, so it divid k'. So we have:

k * len + posFinal = k' * incr

exists k



(k * incr + posCurrent) [len] = posFinal [len]

-}

finalForm :: Integer -> Arith -> Integer
finalForm power (Add [Mul [Var, Lit a], Lit b] :%: Lit m) = let
  (a', b', _, _) = fastMatrixPower power m (a, b, 0, 1)
  in (a' * 2020 + b') `mod` m
finalForm _ _ = error "Equation is not of the form (a * x + b) `mod` m"

fastMatrixPower 0 _ _ = (1, 0, 0, 1)
fastMatrixPower 1 m (a, b, c, d) = (a `mod` m, b `mod` m, c `mod` m, d `mod` m)
fastMatrixPower n m v = do
  let
    approximateSqrt = truncate @Double $ sqrt $ fromIntegral n
    rest = n - (approximateSqrt * approximateSqrt)

    sqrtMatrix = fastMatrixPower approximateSqrt m $ fastMatrixPower approximateSqrt m v
  sqrtMatrix `matrixMul` (fastMatrixPower rest m v)

matrixMul (a, b,
           c, d) (a', b',
                  c', d') = (a * a' + b * c', a * b' + b * d',
                             c * a' + d * c', c * b' + d * d')


day' instructions = finalForm nTimes $ simplify' $ foo' deckSize instructions
  where
    nTimes :: Integer
    nTimes = 101741582076661

    deckSize :: Integer
    deckSize = 119315717514047
{-



Add [Mul [Var,Lit 41443368465112],Lit 58211516997988] :%: Lit 119315717514047

which is:

    ((x * A) + B) % C

We can ignore the % C

    (x * A + B)

    (x, 1) * (A, B)
             (0, 1)
-}

-- IS TOO LOW: 51703269449209
-- TRY       : 46938179068110. Too low too
-- TRY       : 58348342289943

ex :: Draw t => [t]
ex = parseContent [fmt|\
deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
|]

ex' :: Draw t => [t]
ex' = parseContent [fmt|\
deal with increment 7
deal with increment 9
cut -2
|]

ex'' :: Draw t => [t]
ex'' = parseContent [fmt|\
cut 6
deal with increment 7
deal into new stack
|]

ex''' :: Draw t => [t]
ex''' = parseContent [fmt|\
deal with increment 7
deal into new stack
deal into new stack
|]

-- 9399 is too high!
-- 9170 is too high!

-- * Tests

test :: Spec
test = do
--  describe "simple examples" $ do
--    it "of first star" $ do
--      day "" `shouldBe` 0
--    it "of second star" $ do
--      day' "" `shouldBe` 0
  describe "works" $ do
    it "on first star" $ do
      day fileContent 10007 2019 `shouldBe` 6289
    it "on second star" $ do
      day' fileContent `shouldBe` 58348342289943
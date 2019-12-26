{-# LANGUAGE FlexibleInstances #-}
module Day22 where

-- start 16:38
-- first star 17:03... I'm an idiot. I read "what's the card at index XXX" instead of "What is the index of card XXX"
-- second star: 20:37. Well, I had to get my modulo arithmetic back

import Utils hiding ((:*:), (:+:))
import Bezout
import Linear hiding (identity, ex)

import Text.Megaparsec

fileContent = parseContent $(getFile)

parseContent = unsafeParse $ Text.Megaparsec.many $ choice
  [ dealWithIncrement <$> ("deal with increment " *> parseNumber)
  , cut <$> ("cut " *> parseNumber @Integer)
  , dealIntoNewStack <$ "deal into new stack\n"
  ]

-- * Generics

-- * Transform function from an offset to the one of the card BEFORE the deal

dealIntoNewStack lenStack posFinal = Lit lenStack :-: posFinal :-: Lit 1

cut n lenStack posFinal = (posFinal :+: Lit n) :%: lenStack

dealWithIncrement n lenStack posFinal = let
  invN = inverseMod n lenStack
  in (posFinal :*: Lit invN) :%: lenStack

-- Custom arithmetic

data Arith
  = Arith :+: Arith
  | Arith :*: Arith
  | Lit Integer
  | Negate Arith
  | Arith :%: Integer
  | Var
  deriving (Show, Eq)

{-# COMPLETE Add, Mul, Lit, Negate, (:%:), Var #-}

pattern Add :: Arith -> Arith -> Arith
pattern Add a b = a :+: b

pattern Mul :: Arith -> Arith -> Arith
pattern Mul a b = a :*: b

pattern (:-:) :: Arith -> Arith -> Arith
pattern a :-: b = a :+: (Negate b)

infixl 6 :+:
infixl 7 :*:
infixl 7 :%:
infixl 6 :-:

simplify :: Arith -> Arith
simplify = \case
  Negate (Negate v) -> simplify v
  l@(Lit _) -> l
  Negate (Add a b) -> Add (Negate (simplify a)) (Negate (simplify b))
  Negate (Mul a b) -> Mul (simplify $ Negate a) (simplify b)
  Negate (Lit a) -> Lit (-a)

  -- Distribute Add with Mul
  Mul (Add a b) c -> Add (Mul (simplify a) (simplify c)) (Mul (simplify b) (simplify c))

  -- bias to the right any tree
  Add (Add a b) c -> Add (simplify a) (Add (simplify b) (simplify c))
  Mul (Mul a b) c -> Mul (simplify a) (Mul (simplify b) (simplify c))
  -- Compact literals
  Add (Lit a) (Add (Lit b) c) -> Add (Lit (a + b)) (simplify c)
  Mul (Lit a) (Mul (Lit b) c) -> Mul (Lit (a * b)) (simplify c)
  Add (Lit a) (Lit b) -> Lit (a + b)
  Mul (Lit a) (Lit b) -> Lit (a * b)

  -- Drop neutral elements
  Add (Lit 0) b -> simplify b
  Mul (Lit 1) b -> simplify b

  Add x y -> Add (simplify x) (simplify y)
  Mul x y -> Mul (simplify x) (simplify y)

  a :%: v -> (simplify $ killMod v a) :%: v
  e@(Negate Var) -> e
  Negate e -> Negate $ simplify e
  Var -> Var


killMod :: Integer -> Arith -> Arith
killMod v = \case
  Lit i -> Lit (i `mod` v)
  Negate e -> Negate $ killMod v e
  Var -> Var
  Add a b -> Add (killMod v a) (killMod v b)
  Mul a b -> Mul (killMod v a) (killMod v b)
  a :%: v'
    | v == v' -> killMod v a
    | otherwise -> error "MODULO ARE NOT THE SAME"
  a :%: e -> a :%: e

eval var = \case
  Lit i -> i
  Negate e -> - eval var e
  Var -> var
  Add a b -> eval var a + eval var b
  Mul a b -> eval var a * eval var b
  a :%: e -> (eval var a) `mod` e

simplify' x = let
  x' = simplify x
  in
  if x == x'
  then x'
  else simplify' x'

foo :: Integer -> [_] -> Arith
foo deckSize problem = foldl' (.) identity (map ($ deckSize) problem) Var


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
finalForm power ((Var :*: Lit a :+: Lit b) :%: m) = let
  V2 (V2 a' b') _ = fastMatrixPower power m (V2 (V2 a b) (V2 0 1))
  in (a' * 2020 + b') `mod` m
finalForm _ _ = error "Equation is not of the form (a * x + b) `mod` m"

fastMatrixPower 0 _ _ = V2 (V2 1 0) (V2 0 1)
fastMatrixPower 1 m mat = (`mod`m) <$$> mat
fastMatrixPower n m v = do
  let
    approximateSqrt = truncate @Double $ sqrt $ fromIntegral n
    rest = n - (approximateSqrt * approximateSqrt)

    sqrtMatrix = fastMatrixPower approximateSqrt m $ fastMatrixPower approximateSqrt m v
  sqrtMatrix !*! (fastMatrixPower rest m v)

-- * FIRST problem
day :: Integer -> Integer -> _ -> Integer
day nbCards lookFor instructions = let
  arith = simplify' $ foo nbCards instructions
  in unsafeFromJust $ find (\x -> eval x arith == lookFor) [0..nbCards-1]

-- * SECOND problem
day' instructions = finalForm nTimes $ simplify' $ foo deckSize instructions
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

ex' = parseContent [fmt|\
deal with increment 7
deal with increment 9
cut -2
|]

ex'' = parseContent [fmt|\
cut 6
deal with increment 7
deal into new stack
|]

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
      day 10007 2019 fileContent `shouldBe` 6289
    it "on second star" $ do
      day' fileContent `shouldBe` 58348342289943

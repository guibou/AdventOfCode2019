module Utils (
    module Utils
  , module Protolude
  , module Protolude.Error
  , module Unsafe
  , HashMap
  , Vector
  , module Data.Function.Memoize
  , describe
  , it
  , Spec
  , hspec
  , shouldBe
  , shouldReturn
  , here
  , hereLit
  , chunksOf
  , genum, GEnum
  , fmt -- From PyF
  ) where

import Protolude
import Unsafe
import Linear

import Generics.Deriving.Enum (genum, GEnum)

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char

import qualified Crypto.Hash.MD5
import Data.ByteString.Base16 (encode)

import Control.Parallel.Strategies (parBuffer, using, rdeepseq)

import Data.List.Split (chunksOf)

import qualified Data.Set as Set
import Data.Set (Set)
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)

import Data.FileEmbed (embedStringFile)

import qualified Data.Vector as V

import Data.Char (toLower)

import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Control.Monad (void)

import Text.Read (readMaybe)

import Data.Function.Memoize
import Test.Hspec
import qualified Data.Text as Text
import Data.String.Here
import Protolude.Error

import PyF

-- So I can use it in the shell
-- dayX <$$> content

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f x = (fmap . fmap) f x

infixl 4 <$$>

-- * Torus enum

-- |
-- >>> data Test = A | B | C | D deriving (Bounded, Enum, Show)
-- >>> succWrap A
-- B
-- >>> succWrap D
-- A
-- >>> predWrap D
-- C
-- >>> predWrap A
-- D
succWrap :: forall t. (Enum t, Bounded t) => t -> t
succWrap = nWrap 1

predWrap :: forall t. (Enum t, Bounded t) => t -> t
predWrap = nWrap (-1)

nWrap :: forall t. (Enum t, Bounded t) => Int -> t -> t
nWrap d e = let idx = fromEnum e
                m = (fromEnum (maxBound :: t)) + 1
            in toEnum ((idx + d) `mod` m)

countItem :: Eq a => a -> [a] -> Int
countItem x l = countIf (==x) l

countIf :: (a -> Bool) -> [a] -> Int
countIf p l = length (filter p l)


bfs :: Ord p => (Set p -> Set p -> Int -> Bool) -> p -> (p -> [p]) -> (Set p, Set p, Int)
bfs stopCriterion start stepFunction = go (Set.singleton start) (Set.empty) 0
  where go todos visited depth
          | stopCriterion todos visited depth = (todos, visited, depth)
          | otherwise = let newSteps = Set.fromList (mconcat (map stepFunction (Set.toList todos)))
                            okSteps = Set.difference newSteps visited

                        in go okSteps (Set.union todos visited) (depth + 1)

md5 :: ByteString -> ByteString
md5 = encode . Crypto.Hash.MD5.hash

parBufferChunks :: NFData t => [t] -> [t]
parBufferChunks l = let chunks = (chunksOf 4096 l)
                    in mconcat chunks `using` parBuffer 20 rdeepseq

--

getFile :: Q Exp
getFile = fmap loc_module qLocation >>= \name -> embedStringFile ("content/" <> map toLower name)

zipIndex :: V.Vector t -> V.Vector (Int, t)
zipIndex v = V.zip (V.enumFromN 0 (V.length v)) v

-- * Parsing

type Parser t = Parsec Void Text t

sc :: Parser ()
sc = L.space (() <$ spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol_ :: Text -> Parser ()
symbol_ s = void (symbol s)

select :: [t] -> [(t, [t])]
select [] = []
select (x:xs) = (x, xs):((x:) <$$> (select xs))

unsafeParse :: Parser t -> Text -> t
unsafeParse p s = case parse p "" s of
  Right res -> res
  Left e -> panic (Text.pack (errorBundlePretty e))

-- Text utils
unsafeRead :: Read t => Text -> t
unsafeRead = unsafeFromJust . readMaybe . Text.unpack

parse2D :: (Text -> a) -> Text -> [[a]]
parse2D f s = map (map f . Text.words) (Text.lines s)

unsafeRead2D :: Read t => Text -> [[t]]
unsafeRead2D = parse2D unsafeRead

unsafeRead1D :: Read t => Text -> [t]
unsafeRead1D = map unsafeRead . Text.words

parse2DGrid :: (Text -> a) -> Text -> Map (Int, Int) a
parse2DGrid f t = Map.fromList $ do
  (y, l) <- zip [0..] (parse2D f t)
  (x, v) <- zip [0..] l

  pure ((x, y), v)

getBounds :: Map (Int, Int) t -> ((Int, Int), (Int, Int))
getBounds g =
  let
  minX = minimum $ map fst $ Map.keys g
  minY = minimum $ map snd $ Map.keys g
  maxX = maximum $ map fst $ Map.keys g
  maxY = maximum $ map snd $ Map.keys g

  in ((minX, minY), (maxX, maxY))

display2DGrid :: Map (Int, Int) Text -> IO ()
display2DGrid g =
  let ((minX, minY), (maxX, maxY)) = getBounds g
  in
  for_ [minY .. maxY] $ \y -> do
    for_ [minX .. maxX] $ \x -> do
      case Map.lookup (x, y) g of
        Nothing -> putStr (" " :: Text)
        Just v -> putStr v
    putStrLn ("" :: Text)

str2DGrid :: Map (Int, Int) Text -> Text
str2DGrid g =
  let ((minX, minY), (maxX, maxY)) = getBounds g
  in
  Text.intercalate "\n" $ flip map [minY .. maxY] $ \y -> do
    Text.stripEnd $ Text.intercalate "" $ flip map [minX .. maxX] $ \x -> do
      case Map.lookup (x, y) g of
        Nothing -> " "
        Just v -> v

flipImage :: Text -> Text
flipImage = Text.unlines . reverse . Text.lines

-- * Tests Utile
thisModuleName :: Q Exp
thisModuleName = do
  ModuleInfo mi <- reifyModule =<< thisModule
  let t = filter ("Day"`isPrefixOf`) $ map (\(Module _ (ModName name)) -> name) mi

  pure (ListE (map (\x -> TupE [LitE (StringL x), VarE (mkName (x ++ ".test"))]) t))

-- Cycle
cyclePred :: forall t. (Enum t, Bounded t) => t -> t
cyclePred o
  | fromEnum o == fromEnum (minBound :: t) = maxBound
  | otherwise = pred o

cycleSucc :: forall t. (Enum t, Bounded t) => t -> t
cycleSucc o
  | fromEnum o == fromEnum (maxBound :: t) = minBound
  | otherwise = succ o

pow10 :: Int -> Int
pow10 a = 10 ^ a

parseNumber :: Num t => Parser t
parseNumber = fromIntegral <$> ((L.signed sc (lexeme L.decimal)) :: Parser Integer)

bisect p bounds = uncurry go bounds
  where
    go a b
      | a + 1 == b = (a, b)
      | p mid = go mid b
      | otherwise = go a mid
      where
        mid = (a + b) `div` 2

-- | Apply f until it become stable
fixpoint f x = let
  x' = f x
  in
  if x == x'
  then x'
  else fixpoint f x'

-- | @fastMatrixPower n m mat@ = (mat ^ n) `mod` m
-- Using a fast power heuristic (so n can be hyper large)
fastMatrixPower 0 _ _ = V2 (V2 1 0) (V2 0 1)
fastMatrixPower 1 m mat = (`mod`m) <$$> mat
fastMatrixPower n m v = do
  let
    approximateSqrt = truncate @Double $ sqrt $ fromIntegral n
    rest = n - (approximateSqrt * approximateSqrt)

    sqrtMatrix = fastMatrixPower approximateSqrt m $ fastMatrixPower approximateSqrt m v
  sqrtMatrix !*! (fastMatrixPower rest m v)

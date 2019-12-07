module IntCode where

import Utils

import Data.Vector as V
import qualified Data.Map as Map

-- I just finished IntCode from Day2. I feel this stuff will be used
-- again, so time for cleaning, making it more generic

runIntCode :: Vector Int -> Int
runIntCode v = (runIntCode' instructionSet_1_2_99 v) ! 0

-- | Run a generic IntCode machine, with instruction set defined by the first 'Map'
runIntCode'
  :: Map Int (Int -> Vector Int -> (Maybe Int, Vector Int))
  -> Vector Int
  -- ^ The input machine
  -> Vector Int
  -- ^ Output machine
runIntCode' instructionSet = curry go 0
  where
    go (pos, v) = case Map.lookup (v ! pos) instructionSet of
      Just instruction -> let
        (pos', v') = instruction pos v
        in case pos' of
             Nothing -> v'
             Just pos'' -> go (pos'', v')
      Nothing -> error $ [fmt|WTF in this computer, case unhandled {v ! pos}|]

-- Instructions

instructionSet_1_2_99 :: Map Int (Int -> Vector Int -> (Maybe Int, Vector Int))
instructionSet_1_2_99 = Map.fromList [(1, instrAdd), (2, instrMul), (99, instrHalt)]


instrBinop op pos v = let
  a = v ! (pos + 1)
  b = v ! (pos + 2)
  newVal = (v ! a) `op` (v ! b)
  pos' = v ! (pos + 3)
  in (Just $ pos + 4, v // [(pos', newVal)])

instrAdd = instrBinop (+)
instrMul = instrBinop (*)
instrHalt _ v = (Nothing, v)

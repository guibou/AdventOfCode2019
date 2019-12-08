module IntCode where

import Utils

import Data.Vector as V
import qualified Data.Map as Map

-- I just finished IntCode from Day2. I feel this stuff will be used
-- again, so time for cleaning, making it more generic

runIntCode :: Vector Int -> Int
runIntCode v = (fst $ runState (runIntCode' instructionSet_1_2_99 v) ([], [])) ! 0

-- | Run a generic IntCode machine, with instruction set defined by the first 'Map'
runIntCode'
  :: Map Int ((Mode, Mode, Mode) -> Int -> Vector Int -> State ([Int], [Int]) (Maybe Int, Vector Int))
  -> Vector Int
  -- ^ The input machine
  -> State ([Int], [Int]) (Vector Int)
  -- ^ Output machine
runIntCode' instructionSet v'' = go (0,v'')
  where
    go (pos, v) = let
      (instrNumber, modeA, modeB, modeC) = decodeMode (v ! pos)
      in case Map.lookup instrNumber instructionSet of
        Just instruction -> do
          (pos', v') <- instruction (modeA, modeB, modeC) pos v
          case pos' of
            Nothing -> pure v'
            Just pos'' -> go (pos'', v')
        Nothing -> error $ [fmt|WTF in this computer, case unhandled {v ! pos} {pos}|]

-- | Similar as 'runIntCode'' however it only returns (lazyly) the output of the machine.
runIntCodeOutput
  :: Map Int ((Mode, Mode, Mode) -> Int -> Vector Int -> State ([Int], [Int]) (Maybe Int, Vector Int))
  -> Vector Int
  -- ^ The input machine
  -> [Int]
  -- ^ Input state
  -> [Int]
  -- ^ Output state
runIntCodeOutput instructionSet v'' initialInput = go (0,v'', initialInput)
  where
    go (pos, v, input) = let
      (instrNumber, modeA, modeB, modeC) = decodeMode (v ! pos)
      in case Map.lookup instrNumber instructionSet of
        Just instruction -> do
          let ((pos', v'), (input', output)) = runState (instruction (modeA, modeB, modeC) pos v) (input, [])
          case pos' of
            Nothing -> output
            Just pos'' -> output Utils.++ go (pos'', v', input')
        Nothing -> error $ [fmt|WTF in this computer, case unhandled {v ! pos} {pos}|]

-- Instructions

instructionSet_1_2_99 :: Map Int ((Mode, Mode, Mode) -> Int -> Vector Int -> State ([Int], [Int]) (Maybe Int, Vector Int))
instructionSet_1_2_99 = Map.fromList [(1, instrAdd), (2, instrMul), (99, instrHalt)]

instructionSet_day5 :: Map Int ((Mode, Mode, Mode) -> Int -> Vector Int -> State ([Int], [Int]) (Maybe Int, Vector Int))
instructionSet_day5 = Map.fromList [(1, instrAdd), (2, instrMul), (99, instrHalt), (3, instr3), (4, instr4)]

instrBinop op (modeA, modeB, Position) pos v = let
  a = readMode modeA v (pos + 1)
  b = readMode modeB v (pos + 2)
  newVal = a `op` b
  pos' = readMode Immediate v (pos + 3) -- WRITE IS NEVER IN IMMEDIATE MODE
  in pure (Just $ pos + 4, v // [(pos', newVal)])
instrBinop _ _ _ _ = error "binop used with immediate mode for output"

instr3 (Position, _, _) pos v = let
  -- Save position are always position, never immedatie
  savePos = readMode Immediate v (pos + 1)
  in do
    i <- readInput
    pure (Just $ pos + 2, v // [(savePos, i)])
instr3 _ _ _ = error "instr3 used in immediate mode"

instr4 (modeA, _, _) pos v = let
  arg = readMode modeA v (pos + 1)
  in do
    writeOutput arg
    pure (Just $ pos + 2, v)

instr5 (modeA, modeB, _) pos v = let
  a = readMode modeA v (pos + 1)
  b = readMode modeB v (pos + 2)

  in
  pure (Just $ if a /= 0 then b else pos + 3, v)

instr6 (modeA, modeB, _) pos v = let
  a = readMode modeA v (pos + 1)
  b = readMode modeB v (pos + 2)

  in
  pure (Just $ if a == 0 then b else pos + 3, v)

instr7 (modeA, modeB, Position) pos v = let
  a = readMode modeA v (pos + 1)
  b = readMode modeB v (pos + 2)
  c = readMode Immediate v (pos + 3)

  in
  pure (Just $ pos + 4, v // [(c, if a < b then 1 else 0)])
instr7 _ _ _ = error "instr7 used in immediate mode"

instr8 (modeA, modeB, Position) pos v = let
  a = readMode modeA v (pos + 1)
  b = readMode modeB v (pos + 2)
  c = readMode Immediate v (pos + 3)

  in
  pure (Just $ pos + 4, v // [(c, if a == b then 1 else 0)])
instr8 _ _ _ = error "instr8 used in immediate mode"

instructionSet_day5' :: Map Int ((Mode, Mode, Mode) -> Int -> Vector Int -> State ([Int], [Int]) (Maybe Int, Vector Int))
instructionSet_day5' = instructionSet_day5 <> Map.fromList [
  (5, instr5),
  (6, instr6),
  (7, instr7),
  (8, instr8)
  ]

lastInstructionSet = instructionSet_day5'

instrAdd = instrBinop (+)
instrMul = instrBinop (*)
instrHalt _ _ v = pure (Nothing, v)

readInput :: State ([Int], [Int]) Int
readInput = do
  -- non exhaustive pattern synonym, we suppose that we have enough input
  ~((x:input), output) <- get
  put (input, output)
  pure x

writeOutput :: Int -> State ([Int], [Int]) ()
writeOutput x = do
  (input, output) <- get
  put (input, x:output)

data Mode = Position | Immediate
  deriving (Show, Eq)

decodeMode :: Int -> (Int, Mode, Mode, Mode)
decodeMode v = (v `mod` 100, modeAt v 100, modeAt v 1000, modeAt v 10000)

modeAt v x = if (v `div` x) `mod` 10 == 0 then Position else Immediate

readMode :: Mode -> Vector Int -> Int -> Int
readMode Immediate v offset = v ! offset
readMode Position v offset = v ! (v ! offset)

test :: Spec
test = do
  describe "decodeMode" $ do
    it "works for full setup" $ do
      decodeMode 11123 `shouldBe` (23, Immediate, Immediate, Immediate)
      decodeMode 10124 `shouldBe` (24, Immediate, Position, Immediate)
      decodeMode 01125 `shouldBe` (25, Immediate, Immediate, Position)
    it "works for partial setup" $ do
      decodeMode 22 `shouldBe` (22, Position, Position, Position)
      decodeMode 123 `shouldBe` (23, Immediate, Position, Position)
      decodeMode 1124 `shouldBe` (24, Immediate, Immediate, Position)

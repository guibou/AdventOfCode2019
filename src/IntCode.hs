module IntCode where

import Utils

import Data.Vector as V
import qualified Data.Map as Map

type Machine t = State ([Int], Int, Vector Int) t

-- I just finished IntCode from Day2. I feel this stuff will be used
-- again, so time for cleaning, making it more generic
readIntCodeOutput instructions v = let
  (_, (_, _, res)) = runState (runIntCode instructions) ([], 0, v)
  in res ! 0

-- | Run a generic IntCode machine, with instruction set defined by the first 'Map'
runIntCode
  :: Map Int ((Mode, Mode, Mode) -> Machine (Maybe [Int]))
  -> Machine [Int]
  -- ^ Output machine
runIntCode instructionSet = go
  where
    go = do
      (instrNumber, modes) <- decodeInstruction
      case Map.lookup instrNumber instructionSet of
        Just instruction -> do
          output <- instruction modes

          case output of
            Nothing -> pure []
            Just value -> do
              values <- go
              pure (value Utils.++ values)
        Nothing -> error $ [fmt|WTF in this computer, case unhandled {instrNumber}|]

-- | Similar as 'runIntCode'' however it only returns (lazyly) the output of the machine.
runIntCodeOutput
  :: Map Int ((Mode, Mode, Mode) -> Machine (Maybe [Int]))
  -> Vector Int
  -- ^ The input machine
  -> [Int]
  -- ^ Input state
  -> [Int]
  -- ^ (Output state, final vector)
runIntCodeOutput instructionSet v'' initialInput = let
  (res, _) = runState (runIntCode instructionSet) (initialInput, 0, v'')
  in res

-- Instructions
decodeInstruction :: Machine (Int, (Mode, Mode, Mode))
decodeInstruction = do
  (_, pos, v) <- get
  pure $ decodeMode (v ! pos)

instructionSet_1_2_99 :: Map Int ((Mode, Mode, Mode) -> Machine (Maybe [Int]))
instructionSet_1_2_99 = Map.fromList [(1, instrAdd), (2, instrMul), (99, instrHalt)]

instructionSet_day5 :: Map Int ((Mode, Mode, Mode) -> Machine (Maybe [Int]))
instructionSet_day5 = Map.fromList [(1, instrAdd), (2, instrMul), (99, instrHalt), (3, instr3), (4, instr4)]

readMemory :: Mode -> Machine Int
readMemory mode = do
  (_, pc, v) <- get
  modifyInstructionPointer (+1)
  pure $ readMode mode v pc

readImmediate :: Machine Int
readImmediate = do
  (_, pc, v) <- get
  modifyInstructionPointer (+1)
  pure $ readMode Immediate v pc

noReturn :: Machine () -> Machine (Maybe [Int])
noReturn x = do
  modifyInstructionPointer (+1)
  x
  pure $ Just []

instrBinop op (modeA, modeB, Position) = noReturn $ do
  a <- readMemory modeA
  b <- readMemory modeB
  pos' <- readImmediate

  alterMemory [(pos', a `op` b)]

instrBinop _ _ = error "binop used with immediate mode for output"

instr3 (Position, _, _) = noReturn $ do
    savePos <- readImmediate
    i <- readInput
    alterMemory [(savePos, i)]

instr3 _ = error "instr3 used in immediate mode"

instr4 (modeA, _, _) = do
  modifyInstructionPointer (+1)
  arg <- readMemory modeA

  pure (Just [arg])

instr5 (modeA, modeB, _) = noReturn $ do
  a <- readMemory modeA
  b <- readMemory modeB

  modifyInstructionPointer $ if a /= 0 then const b else identity

instr6 (modeA, modeB, _) = noReturn $ do
  a <- readMemory modeA
  b <- readMemory modeB

  modifyInstructionPointer $ if a == 0 then const b else identity

instr7 (modeA, modeB, Position) = noReturn $ do
  a <- readMemory modeA
  b <- readMemory modeB
  c <- readImmediate

  alterMemory [(c, if a < b then 1 else 0)]

instr7 _ = error "instr7 used in immediate mode"

instr8 (modeA, modeB, Position) = noReturn $ do
  a <- readMemory modeA
  b <- readMemory modeB
  c <- readImmediate

  alterMemory [(c, if a == b then 1 else 0)]
instr8 _ = error "instr8 used in immediate mode"

instructionSet_day5' :: Map Int ((Mode, Mode, Mode) -> Machine (Maybe [Int]))
instructionSet_day5' = instructionSet_day5 <> Map.fromList [
  (5, instr5),
  (6, instr6),
  (7, instr7),
  (8, instr8)
  ]

lastInstructionSet = instructionSet_day5'

instrAdd = instrBinop (+)
instrMul = instrBinop (*)
instrHalt _ = pure Nothing

-- * Machine instructions

readInput :: Machine Int
readInput = do
  -- non exhaustive pattern synonym, we suppose that we have enough input
  (input, pc, memory) <- get
  case input of
    (x:xs) -> do
      put (xs, pc, memory)
      pure x
    _ -> error "No enough input"

modifyInstructionPointer :: (Int -> Int) -> Machine ()
modifyInstructionPointer f = do
  (input, pc, memory) <- get
  put (input, f pc, memory)

alterMemory :: [(Int, Int)] -> Machine ()
alterMemory changes = do
  (input, pc, memory) <- get
  put (input, pc, memory // changes)

-- * modes

data Mode = Position | Immediate
  deriving (Show, Eq)

decodeMode :: Int -> (Int, (Mode, Mode, Mode))
decodeMode v = (v `mod` 100, (modeAt v 100, modeAt v 1000, modeAt v 10000))

modeAt v x = if (v `div` x) `mod` 10 == 0 then Position else Immediate

readMode :: Mode -> Vector Int -> Int -> Int
readMode Immediate v offset = v ! offset
readMode Position v offset = v ! (v ! offset)

test :: Spec
test = do
  describe "decodeMode" $ do
    it "works for full setup" $ do
      decodeMode 11123 `shouldBe` (23, (Immediate, Immediate, Immediate))
      decodeMode 10124 `shouldBe` (24, (Immediate, Position, Immediate))
      decodeMode 01125 `shouldBe` (25, (Immediate, Immediate, Position))
    it "works for partial setup" $ do
      decodeMode 22 `shouldBe` (22, (Position, Position, Position))
      decodeMode 123 `shouldBe` (23, (Immediate, Position, Position))
      decodeMode 1124 `shouldBe` (24, (Immediate, Immediate, Position))

module IntCode where

{-
TODO:
- fix the crappy lazyness leak
- fix the instruction set. Some places hardcode "lastInstructionSet" and this should be simplified
-}

import Utils

import Data.Vector as V
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Text.Megaparsec as P


type MachineState t = (Int, HashMap Int t, Int)
type Machine intType t = State (MachineState intType) t

data MachineResult t = Terminate | Output t (MachineResult t) | Continuation (t -> MachineResult t)

showMachineResult :: MachineResult Int -> Text
showMachineResult = \case
  Terminate -> [fmt|Terminate|]
  Output t xs -> [fmt|Output {t} {showMachineResult xs}|]
  Continuation _ -> "Continuation _"


type MachineStream t = MachineResult t -> MachineResult t

{-# SPECIALIZE readIntCodeOutput ::Map Int ((Mode, Mode, Mode) -> Machine Int (MachineStream Int)) -> Vector Int -> Int #-}

-- I just finished IntCode from Day2. I feel this stuff will be used
-- again, so time for cleaning, making it more generic
readIntCodeOutput :: (Show t, Integral t) => Map Int ((Mode, Mode, Mode) -> Machine t (MachineStream t)) -> Vector t -> t
readIntCodeOutput instructions v = let
  (_, (_, res, _)) = runState (runIntCode instructions) (0, HashMap.fromList (Utils.zip [0..] (V.toList v)), 0)
  in res HashMap.! 0

-- {-# SPECIALIZE runIntCode :: Map Int ((Mode, Mode, Mode) -> Machine Int (MachineStream Int)) -> Machine Int [Int] #-}
  -- ^ Output machine
-- | Run a generic IntCode machine, with instruction set defined by the first 'Map'
runIntCode
  :: Integral t => Map Int ((Mode, Mode, Mode) -> Machine t (MachineStream t))
  -> Machine t (MachineResult t)
  -- ^ Output machine
runIntCode instructionSet = go
  where
    go = do
      (instrNumber, modes) <- decodeInstruction
      -- There is a crappy lazyness leak here
      -- to be discussed later
      if instrNumber == 99
        then pure Terminate
        else
        case Map.lookup instrNumber instructionSet of
          Just instruction -> do
            output <- instruction modes
            output <$> go
          Nothing -> error $ [fmt|WTF in this computer, case unhandled {instrNumber}|]

{-# SPECIALIZE runIntCodeOutput :: Map Int ((Mode, Mode, Mode) -> Machine Int (MachineStream Int)) -> Vector Int -> [Int] -> [Int] #-}

-- | Similar as 'runIntCode'' however it only returns (lazyly) the output of the machine.
runIntCodeOutput
  :: Integral t
  => Map Int ((Mode, Mode, Mode) -> Machine t (MachineStream t))
  -> Vector t
  -- ^ The input machine
  -> [t]
  -- ^ Input state
  -> [t]
  -- ^ (Output state, final vector)
runIntCodeOutput instructionSet v'' initialInput = let
  (res, _) = runState (runIntCode instructionSet) (0, HashMap.fromList (Utils.zip [0..] (V.toList v'')), 0)
  in consumeContinuation res initialInput

consumeContinuation :: Integral t => MachineResult t -> [t] -> [t]
consumeContinuation Terminate _ = []
consumeContinuation (Output v xs) input = v : consumeContinuation xs input
consumeContinuation (Continuation f) inputs = do
  case inputs of
    (x:xs) -> do
      consumeContinuation (f x) xs
    [] -> error "Not enough input"

startStreamingMachine :: Integral t => Vector t -> MachineResult t
startStreamingMachine v'' = fst $ runState (runIntCode lastInstructionSet) (0, HashMap.fromList (Utils.zip [0..] (V.toList v'')), 0)



{-# SPECIALIZE decodeInstruction :: Machine Int (Int, (Mode, Mode, Mode)) #-}

-- Instructions
decodeInstruction :: Integral t => Machine t (Int, (Mode, Mode, Mode))
decodeInstruction = do
  (pos, v, _) <- get
  pure $ decodeMode (fromIntegral (v HashMap.! pos))

instructionSet_1_2_99 :: Integral t => Map Int ((Mode, Mode, Mode) -> Machine t (MachineStream t))
instructionSet_1_2_99 = Map.fromList [(1, instrAdd), (2, instrMul), (99, instrHalt)]

instructionSet_day5 :: Integral t => Map Int ((Mode, Mode, Mode) -> Machine t (MachineStream t))
instructionSet_day5 = Map.fromList [(1, instrAdd), (2, instrMul), (99, instrHalt), (3, instr3), (4, instr4)]

{-# SPECIALIZE readMemory :: Mode -> Machine Int Int #-}

readMemory :: Integral t => Mode -> Machine t t
readMemory mode = do
  (pc, v, relBase) <- get
  modifyInstructionPointer (+1)
  pure $ readMode relBase mode v pc

{-# SPECIALIZE readImmediate :: AbsOrRel -> Machine Int Int #-}

readImmediate :: Integral t => AbsOrRel -> Machine t t
readImmediate absOrRel = do
  (pc, v, relBase) <- get
  modifyInstructionPointer (+1)
  pure $ (readMode relBase Immediate v pc + case absOrRel of
             Absolute -> 0
             Relative -> fromIntegral relBase)

noReturn :: Machine t () -> Machine t (MachineStream t)
noReturn x = do
  modifyInstructionPointer (+1)
  x
  pure $ identity

{-# SPECIALIZE instrBinop :: (Int -> Int -> Int) -> (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}

instrBinop :: Integral t => (t -> t -> t) -> (Mode, Mode, Mode) -> Machine t (MachineStream t)
instrBinop op (modeA, modeB, Position rel) = noReturn $ do
  a <- readMemory modeA
  b <- readMemory modeB
  pos' <- readImmediate rel

  alterMemory (fromIntegral pos', a `op` b)

instrBinop _ _ = error "binop used with immediate mode for output"

instr3 (Position rel, _, _) = do
    modifyInstructionPointer (+1)
    savePos <- readImmediate rel

    st <- get

    pure $ const $ Continuation $ \i -> fst $ flip runState st $ do
      alterMemory (fromIntegral savePos, i)
      runIntCode lastInstructionSet

instr3 _ = error "instr3 used in immediate mode"

instr4 (modeA, _, _) = do
  modifyInstructionPointer (+1)
  arg <- readMemory modeA

  pure (Output arg)

instr5 (modeA, modeB, _) = noReturn $ do
  a <- readMemory modeA
  b <- readMemory modeB

  modifyInstructionPointer $ if a /= 0 then const (fromIntegral b) else identity

instr6 (modeA, modeB, _) = noReturn $ do
  a <- readMemory modeA
  b <- readMemory modeB

  modifyInstructionPointer $ if a == 0 then const (fromIntegral b) else identity

instr7 (modeA, modeB, Position rel) = noReturn $ do
  a <- readMemory modeA
  b <- readMemory modeB
  c <- readImmediate rel

  alterMemory (fromIntegral c, if a < b then 1 else 0)

instr7 _ = error "instr7 used in immediate mode"

instr8 (modeA, modeB, Position rel) = noReturn $ do
  a <- readMemory modeA
  b <- readMemory modeB
  c <- readImmediate rel

  alterMemory (fromIntegral c, if a == b then 1 else 0)
instr8 _ = error "instr8 used in immediate mode"

instr9 (modeA, _, _) = noReturn $ do
  relOffset <- readMemory modeA

  (pc, v, relBase) <- get

  put (pc, v, relBase + fromIntegral relOffset)

instructionSet_day5' :: Integral t => Map Int ((Mode, Mode, Mode) -> Machine t (MachineStream t))
instructionSet_day5' = instructionSet_day5 <> Map.fromList [
  (5, instr5),
  (6, instr6),
  (7, instr7),
  (8, instr8)
  ]

instructionSet_day9 :: Integral t => Map Int ((Mode, Mode, Mode) -> Machine t (MachineStream t))
instructionSet_day9 = instructionSet_day5' <> Map.fromList [(9, instr9)]

lastInstructionSet :: Integral t => Map Int ((Mode, Mode, Mode) -> Machine t (MachineStream t))
lastInstructionSet = instructionSet_day9

{-# SPECIALIZE instrAdd :: (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}
{-# SPECIALIZE instrMul :: (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}
{-# SPECIALIZE instrHalt :: (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}
{-# SPECIALIZE instr3 :: (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}
{-# SPECIALIZE instr4 :: (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}
{-# SPECIALIZE instr5 :: (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}
{-# SPECIALIZE instr6 :: (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}
{-# SPECIALIZE instr7 :: (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}
{-# SPECIALIZE instr8 :: (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}
{-# SPECIALIZE instr9 :: (Mode, Mode, Mode) -> Machine Int (MachineStream Int) #-}

instrAdd, instrMul, instrHalt, instr3, instr4, instr5, instr6, instr7, instr8, instr9 :: Integral t => (Mode, Mode, Mode) -> Machine t (MachineStream t)
instrAdd = instrBinop (+)
instrMul = instrBinop (*)
instrHalt _ = pure $ const Terminate

-- * Machine instructions

modifyInstructionPointer :: (Int -> Int) -> Machine t ()
modifyInstructionPointer f = do
  (pc, memory, relBase) <- get
  put (f pc, memory, relBase)

alterMemory :: Integral t => (Int, t) -> Machine t ()
alterMemory (offset, val) = do
  (pc, memory, relBase) <- get

  put (pc, HashMap.insert offset val memory, relBase)

-- * modes

data AbsOrRel = Absolute | Relative
  deriving (Show, Eq)

data Mode = Position AbsOrRel | Immediate
  deriving (Show, Eq)

decodeMode :: Int -> (Int, (Mode, Mode, Mode))
decodeMode v = (v `mod` 100, (modeAt v 100, modeAt v 1000, modeAt v 10000))

modeAt :: Int -> Int -> Mode
modeAt v x = case (v `div` x) `mod` 10 of
  0 -> Position Absolute
  1 -> Immediate
  2 -> Position Relative
  _ -> error "WTF non exhaustive mode"

{-# SPECIALIZE readMode :: Int -> Mode -> HashMap Int Int -> Int -> Int #-}

readMode :: Integral t => Int -> Mode -> HashMap Int t -> Int -> t
readMode _ Immediate v offset = v `readSafe` offset
readMode _ (Position Absolute) v offset = v `readSafe` fromIntegral (v `readSafe` offset)
readMode relativeBase (Position Relative) v offset = v `readSafe` (fromIntegral (v `readSafe` offset) + relativeBase)

readSafe v offset = case HashMap.lookup offset v of
  Just res -> res
  Nothing -> 0

test :: Spec
test = do
  describe "decodeMode" $ do
    it "works for full setup" $ do
      decodeMode 11123 `shouldBe` (23, (Immediate, Immediate, Immediate))
      decodeMode 10124 `shouldBe` (24, (Immediate, Position Absolute, Immediate))
      decodeMode 01125 `shouldBe` (25, (Immediate, Immediate, Position Absolute))
    it "works for partial setup" $ do
      decodeMode 22 `shouldBe` (22, (Position Absolute, Position Absolute, Position Absolute))
      decodeMode 123 `shouldBe` (23, (Immediate, Position Absolute, Position Absolute))
      decodeMode 1124 `shouldBe` (24, (Immediate, Immediate, Position Absolute))

parseIntCode :: Integral t => Text -> V.Vector t
parseIntCode = V.fromList . unsafeParse (parseNumber `P.sepBy` ",")

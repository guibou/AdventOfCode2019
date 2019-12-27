module Day23 where

-- start 00:01. Pause 00:04. Reprise 00:14
-- My lazy implementation won't work here... I'm sad ;)
-- Restart at 11:50 after refactoring of IntCode. I'm out of time
-- 12:17 star 1
-- I really had issues with that one to understand the "idle" logic.

import qualified Data.Map as Map
import Utils
import IntCode

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = parseIntCode @Int

-- * Generics
sendMessage (x, y) (Continuation c)
  = let Continuation c2 = c x in c2 y

-- * FIRST problem
network :: _ -> Bool -> Int
network code stopOnFirstPacketToNAT = do
  let
    initMachines = flip map [0..49] $ \addr -> do
      case startStreamingMachine code of
        Continuation cc -> cc addr
        _ -> error "WTF on this machine"

    f :: (([MachineResult Int], Map Int [(Int, Int)], Int)) -> (Int, MachineResult Int) -> ([MachineResult Int],Map Int [(Int, Int)], _)
    f (acc, chan, !idleCount) (addrCurrent, machine) = case machine of
      Continuation cont -> case Map.lookup addrCurrent chan of
        Just (message:messages) -> let m' = sendMessage message (Continuation cont)
                                   in (m':acc, Map.insert addrCurrent messages chan, idleCount)
        _ -> ((cont (-1)):acc, chan, idleCount + 1)
      Output addr (Output x (Output y c)) -> (c:acc, Map.insertWith (++) addr [(x, y)] chan, idleCount)
      Terminate -> error "End of machine"
      Output _ _ -> error "Weirdly formatted output"

    go
      :: _
      -> Map Int [(Int, Int)]
      -- ^ The list of messages
      -> Maybe (Int, Int)
      -- ^ last packet sent to NAT (255)
      -> Maybe Int
      -- ^ last Y sent by the NAT
      -> Int
    go machines chan lastPacket lastYSent = do
      let (machines', chan', idleCount) = foldr (flip f) ([], chan, 0) (zip [0..] machines)

          (chan'', lastPacket') = case Map.lookup 255 chan' of
            Nothing -> (chan', lastPacket)
            Just [p] -> do
              (Map.delete 255 chan', Just p)
            Just _ -> error "Received more than one message on 255"

          (chan''', lastYSent')
              | idleCount == 50, Just lastPacketV@(_, y) <- lastPacket = do
                      (Map.singleton 0 [lastPacketV], Just y)
              | otherwise = (chan'', lastYSent)

      if
        -- 1 star: stop on the first package received by NAT
        | stopOnFirstPacketToNAT, Just (_, y) <- lastPacket -> y
        -- 2 star: stop at first package sent by NAT (on idle) twice in a row
        | idleCount == 50, Just y <- lastYSent, Just (_, y') <- lastPacket, y == y' -> y
        | otherwise -> go machines' chan''' lastPacket' lastYSent'

  go initMachines (Map.empty) Nothing Nothing

day code = network code True

day' code = network code False

-- 2251799 is too high
-- -1 is not correct
-- 34929 is not correct
-- 13165 is not correct

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 20160
    it "on second star" $ do
      day' fileContent `shouldBe` 13164

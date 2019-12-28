module Day25 where

import Utils
import IntCode
import qualified Data.Text as Text
import Data.Char (isDigit)

-- start 17:20

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = parseIntCode @Int

pathToSecurityCheck = Text.lines $ [fmt|
south
take fixed point
north
west
west
west
take hologram
east
east
east
north
take candy cane
west
take antenna
west
take shell
east
east
north
north
take polygon
south
west
take fuel cell
west|]

-- "take fixed point","take candy cane","take shell","take polygon","west"

combination [] = [[]]
combination (x:xs) = do
  l <- combination xs
  [x:l, l]

allItems :: [Text]
allItems = ["fixed point", "hologram", "candy cane", "antenna", "shell", "polygon", "fuel cell"]

sendMessage message machine = consumeToContinuation $ feedContinuation (map ord (Text.unpack message ++ ['\n'])) machine
sendMessages [x] machine = sendMessage x machine
sendMessages (x:xs) machine = let (_, machine') = sendMessage x machine
                              in sendMessages xs machine'

interactive machine = do
  line <- getLine
  let (message, machine') = sendMessage line machine
  putStrLn message
  interactive machine'

-- * Generics
day :: Monad m => (IO () -> m ()) -> _ -> m Int
day debug code = do
  let
    machine = startStreamingMachine code

    bruteForceSecurity machine = go machine (combination allItems)
      where
        go machine [] = error "WTF no solution"
        go machine (items: itemss) = do
          -- TODO: optimisation here, we can avoid some drops ;)
          let messages = (map ("drop " <>) allItems) <> (map ("take " <>) items) <> ["west"]
          debug $ print messages
          let (lastMessage, machine') = sendMessages messages machine

          if not ("Security Checkpoint" `Text.isInfixOf` lastMessage)
            then do
              let n = Text.takeWhile isDigit $ Text.dropWhile (not . isDigit) lastMessage
              pure (unsafeRead n)
            else go machine' itemss

    go machine [] = bruteForceSecurity machine
    go machine (action:actions) = do
      let (display, machine') = sendMessage action machine

      debug $ putStrLn display
      go machine' actions

  let (display, machine') = consumeToContinuation machine
  debug $ putStrLn display
  go machine' pathToSecurityCheck

consumeToContinuation m = case m of
  Output i ix -> let (is, c) = consumeToContinuation ix
                 in (Text.pack [chr i] <> is, c)
  Continuation c -> ("", Continuation c)
  Terminate -> ("", Terminate)

feedContinuation [] m = m
feedContinuation (x:xs) (Continuation f) = feedContinuation xs (f x)
feedContinuation _ Terminate = error "WTF terminate"

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day (const (pure ())) fileContent `shouldReturn` 136839232




{-

                                                                                                     #
                                                                                                # engineering* #
                                                                                                     |
                                             pressure floor - security check point - crew carter - holodeck
                                                                                                      |
                             #           #             #                   #           #              |
                          storage - hallway - hot chocotalt # science lab-navigation   - kitchen  -   coridor* #
                       #     #          |              #       #           |                         |
                                        |                #passengers ---- arcade#
                                        |                      #            #
                        #               |
                   #stables*   -  observatory   - slick bay  ----------------------------------- hull breach#
                       #                #                                                            |
                                                                                             # gift wrapping center* #
                                                                                                    #

NAVIGATION: fifty stars to recalibrate

stables: take *hologram*
kitchen: antenna
navigation: take shell

arcade: NOT take giant electromagnet

scence-lab dont take the escape pod
observatory: don't take the infinite loop
hot chocolate: don't take *photons*
storage: don't take *molten lava*



Items in your inventory:
- hologram
- shell
- fuel cell
- fixed point
- polygon
- antenna
- candy cane

--

- hologram
- shell
- fuel cell
- fixed point

is too heavy

- hologram
- shell
- fuel cell
- polygon
- candy cane
-








south
take fixed point
north
west
west
west
take hologram
east
east
east
north
take candy cane
west
take antenna
west
take shell
east
east
north
north
take polygon
south
west
take fuel cell
west




-}

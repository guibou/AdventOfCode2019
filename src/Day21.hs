module Day21 where

-- start 23:58. I just copied the int code and go to bed. Not read the instructions. Pause.
-- start 14:53
-- First start 15:53. I loved it.
-- Secnod start 16:37 ;)

import Utils
import IntCode

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = parseIntCode @Int

-- * Generics
{-
00 .... -> DEAD
01 ...# -> J
02 ..#. -> DEAD
03 ..## -> J
04 .#.. -> DEAD
05 .#.# -> J
06 .##. -> DEAD
07 .### -> J
08 #... -> WALK
09 #..# -> J
10 #.#. -> W
11 #.## -> J
12 ##.. -> W
13 ##.# -> J
14 ###. -> W
15 #### -> J

A -> On avance

D & !(A&B) -> J


..#.

A!B!CD


!B!C & D

D & (! B | ! C | !A)



Second solution.

Jump if D is good AND

D AND (H OR E)


###.##.###    ACD, BCD
^^  ^^
####.#####    BCD
 ^^^
####..#.##
  ^   ^

#..##.##.#

-}



-- * FIRST problem
day :: _ -> _
day code = do
  let
    input = map ord [fmt|\
NOT B T
NOT A J
OR J T
NOT C J
OR J T
AND D T
NOT T J
NOT J J
WALK
|]
    machine = runIntCodeOutput @Int code input
  unsafeLast machine

-- * SECOND problem
{-

(H OR E) AND D ->

######.#########     !A & D
####..##.##.####
  ^   ^   ^
######..#.######     D & !E


Tu saute si tu sais que tu peux resauter ou avancer et resauter

D & (H | (E & I) | (E & F) & !(A & B & C)

ABCDEFGHI
   *   *#
   **   *
   ***
#.##
-}
day' :: _ -> _
day' code = do
  let
    input = map ord [fmt|\
OR E T
AND I T
OR E J
AND F J
OR J T
OR H T
AND D T
NOT T J
NOT J J
NOT D T
OR A T
AND B T
AND C T
NOT T T
AND T J
RUN
|]
    machine = runIntCodeOutput @Int code input
  unsafeLast machine
-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 19362822
    it "on second star" $ do
      day' fileContent `shouldBe` 1143625214

module All where

import Utils

import Weigh

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12 -- Not full
import Day13
import Day14
import Day15
import Day16 -- star 2 is not tested because too slow right now
import Day17
import Day18 -- Super slow
import Day19
import Day20
import Day21
import Day22
-- import Day23 -- Not Finished star2
import Day24
import Day25

allDays = sortBy (comparing fst) $(thisModuleName)

-- Test all samples and return a timing for all
tests = hspec $ mapM_ (\(name, s) -> describe name s) $ allDays

-- Test all samples and return a timing for each
tests' = mapM_ (\(name, s) -> hspec $ describe name s) $ allDays

-- Test all samples and return a timing for each
testsWeigh = mainWith $ mapM_ (\(name, s) -> action name $ hspec $ describe name s) allDays

module All where

import Utils

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7

-- Test all samples and return a timing for all
tests = hspec $ mapM_ (\(name, s) -> describe name s) $(thisModuleName)

-- Test all samples and return a timing for each
tests' = mapM_ (\(name, s) -> hspec $ describe name s) $(thisModuleName)

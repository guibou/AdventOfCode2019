module All where

import Utils

import Day1
import Day2
import Day3

test = hspec $ mapM_ (\(name, s) -> describe name s) $(thisModuleName)

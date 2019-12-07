module All where

import Utils

import Day1

test = hspec $ mapM_ (\(name, s) -> describe name s) $(thisModuleName)

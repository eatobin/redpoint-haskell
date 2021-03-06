{-# OPTIONS -Wall #-}

module All_Tests where

import           Hat_Test
import           Roster_Test
import           Rules_Test
import           Test.HUnit

runAllTests :: IO Counts
runAllTests = runTestTT $ TestList [ rosterTests, hatTests, rulesTests ]

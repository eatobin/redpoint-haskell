module All_Tests where

import           Hat
import           Hat_Test
import           Roster_Test
import           Test.HUnit

runAllTests = runTestTT $ TestList [ rosterTests, hatTests ]

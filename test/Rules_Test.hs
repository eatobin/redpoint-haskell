module Rules_Test where

import           Hat
import           Roster
import           Roster_Utility
import           Rules
import           Test.HUnit

rulesTests = TestList []

runRulesTests = runTestTT $ TestList [ rulesTests ]

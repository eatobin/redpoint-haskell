module Rules_Test where

import           Hat
import           Roster
import           Roster_Test
import           Roster_Utility
import           Rules
import           Test.HUnit

testGiveeNotSelf = (~=?)
  True
  (giveeNotSelf "JohLen" "GeoHar")

testGiveeNotRecipPass = (~=?)
  True
  (giveeNotRecip "JohLen" "GeoHar" 0 roster)

testGiveeNotRecipFail = (~=?)
  False
  (giveeNotRecip "JohLen" "GeoHar" 0 recipRoster)

rulesTests = TestList [ testGiveeNotSelf, testGiveeNotRecipPass
                      , testGiveeNotRecipFail ]

runRulesTests = runTestTT $ TestList [ rulesTests ]

module Hat_Test where

import           Hat
import           Roster
import           Roster_Test
import           Roster_Utility
import           Test.HUnit

testHat = ["GeoHar","JohLen","PauMcc","RinSta"]

testMakeHat = (~=?)
  testHat
  (makeHat roster)

testRemovePuck = (~=?)
  ["GeoHar","PauMcc","RinSta"]
  (removePuck "JohLen" testHat)

testDiscardPuck = (~=?)
  ["GeoHar","PauMcc"]
  (discardPuck "GeoHar" ["PauMcc"])

testReturnDiscards = (~=?)
  ["PauMcc","JohLen","GeoHar"]
  (returnDiscards ["GeoHar"] ["PauMcc","JohLen"])

hatTests = TestList [ testMakeHat, testRemovePuck, testDiscardPuck
                    , testReturnDiscards ]

runHatTests = runTestTT $ TestList [ hatTests ]

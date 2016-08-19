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

testRemovePuckEmpty = (~=?)
  []
  (removePuck "JohLen" [])

testDiscardPuck = (~=?)
  ["PauMcc","JohLen"]
  (discardPuck "JohLen" ["PauMcc"])

testReturnDiscards = (~=?)
  ["PauMcc","JohLen","GeoHar"]
  (returnDiscards ["GeoHar"] ["PauMcc","JohLen"])

hatTests = TestList [ testMakeHat, testRemovePuck, testDiscardPuck
                    , testReturnDiscards, testRemovePuckEmpty ]

runHatTests = runTestTT $ TestList [ hatTests ]
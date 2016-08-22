module Hat_Test where

import           Hat
import           Roster
import           Roster_Test
import           Roster_Utility
import           Test.HUnit

testHat = ["GeoHar","JohLen","PauMcc","RinSta"]

testMakeHat = (~=?)
  testHat
  (makeHatGivee roster)

testRemovePuck = (~=?)
  ["GeoHar","PauMcc","RinSta"]
  (removePuckGivee "JohLen" testHat)

testRemovePuckEmpty = (~=?)
  []
  (removePuckGivee "JohLen" [])

testDiscardPuck = (~=?)
  ["PauMcc","JohLen"]
  (discardPuckGivee "JohLen" ["PauMcc"])

testReturnDiscards = (~=?)
  ["PauMcc","JohLen","GeoHar"]
  (returnDiscards ["GeoHar"] ["PauMcc","JohLen"])

hatTests = TestList [ testMakeHat, testRemovePuck, testDiscardPuck
                    , testReturnDiscards, testRemovePuckEmpty ]

runHatTests = runTestTT $ TestList [ hatTests ]

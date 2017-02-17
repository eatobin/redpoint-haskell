module Hat_Test where

import           Hat
import           Roster_Test
import           Test.HUnit

testHat :: [[Char]]
testHat = ["GeoHar","JohLen","PauMcc","RinSta"]

testMakeHat :: Test
testMakeHat = (~=?)
  testHat
  (makeHatGivee roster)

testRemovePuck :: Test
testRemovePuck = (~=?)
  ["GeoHar","PauMcc","RinSta"]
  (removePuckGivee "JohLen" testHat)

testRemovePuckEmpty :: Test
testRemovePuckEmpty = (~=?)
  []
  (removePuckGivee "JohLen" [])

testDiscardPuck :: Test
testDiscardPuck = (~=?)
  ["PauMcc","JohLen"]
  (discardPuckGivee "JohLen" ["PauMcc"])

testReturnDiscards :: Test
testReturnDiscards = (~=?)
  ["PauMcc","JohLen","GeoHar"]
  (returnDiscards ["GeoHar"] ["PauMcc","JohLen"])

hatTests :: Test
hatTests = TestList [ testMakeHat, testRemovePuck, testDiscardPuck
                    , testReturnDiscards, testRemovePuckEmpty ]

runHatTests :: IO Counts
runHatTests = runTestTT $ TestList [ hatTests ]

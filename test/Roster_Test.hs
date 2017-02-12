module Roster_Test where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Roster
import           Roster_Utility
import           Test.HUnit

plr1 :: Player
plr1 = Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {giver = "GeoHar", givee = "JohLen"}]}

roster :: Map.Map String Player
roster = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]

rosterGE :: Map.Map String Player
rosterGE = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "PauMcc"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]

rosterGR :: Map.Map String Player
rosterGR = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "RinSta", giver = "GeoHar"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]
rosterAddYear :: Map.Map String Player
rosterAddYear = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"},GiftPair {givee = "none", giver = "none"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"},GiftPair {givee = "none", giver = "none"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"},GiftPair {givee = "none", giver = "none"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"},GiftPair {givee = "none", giver = "none"}]})]
testRosterList :: [[String]]
testRosterList = [["The Beatles","2014"],["RinSta","Ringo Starr","JohLen","GeoHar"],["JohLen","John Lennon","PauMcc","RinSta"],["GeoHar","George Harrison","RinSta","PauMcc"],["PauMcc","Paul McCartney","GeoHar","JohLen"]]

testGetRosterName :: Test
testGetRosterName = (~=?)
  "The Beatles"
  (getRosterName testRosterList)

testGetRosterYear :: Test
testGetRosterYear = (~=?)
  2014
  (getRosterYear testRosterList)

testMakeplayersMap :: Test
testMakeplayersMap = (~=?)
  roster
  (makePlayersMap testRosterList)

testGetPlayerInRoster :: Test
testGetPlayerInRoster = (~=?)
  plr1
  (getPlayerInRoster "RinSta" roster)

testGetPlayerNameInRoster :: Test
testGetPlayerNameInRoster = (~=?)
  "Ringo Starr"
  (getPlayerNameInRoster "RinSta" roster)

testGetGiveeInRoster :: Test
testGetGiveeInRoster = (~=?)
  "GeoHar"
  (getGiveeInRoster "PauMcc" roster 0)

testSetGiveeInRosterPass :: Test
testSetGiveeInRosterPass = (~=?)
  rosterGE
  (setGiveeInRoster "GeoHar" 0 "GeoHar" roster)

testSetGiveeInRosterFailPlr :: Test
testSetGiveeInRosterFailPlr = (~=?)
  roster
  (setGiveeInRoster "GeoHarX" 0 "GeoHar" roster)

testSetGiveeInRosterFailYr :: Test
testSetGiveeInRosterFailYr = (~=?)
  roster
  (setGiveeInRoster "GeoHar" 9 "GeoHar" roster)

testSetGiveeInRosterFailGv ::Test
testSetGiveeInRosterFailGv = (~=?)
  roster
  (setGiveeInRoster "GeoHar" 0 "GeoHarX" roster)

testSetGiverInRosterPass ::Test
testSetGiverInRosterPass = (~=?)
  rosterGR
  (setGiverInRoster "GeoHar" 0 "GeoHar" roster)

testSetGiverInRosterFailPlr :: Test
testSetGiverInRosterFailPlr = (~=?)
  roster
  (setGiverInRoster "GeoHarX" 0 "GeoHar" roster)

testSetGiverInRosterFailYr :: Test
testSetGiverInRosterFailYr = (~=?)
  roster
  (setGiverInRoster "GeoHar" 9 "GeoHar" roster)

testSetGiverInRosterFailGv :: Test
testSetGiverInRosterFailGv = (~=?)
  roster
  (setGiverInRoster "GeoHar" 0 "GeoHarX" roster)

testAddYearInRoster :: Test
testAddYearInRoster = (~=?)
  rosterAddYear
  (addYearInRoster roster)

rosterTests :: Test
rosterTests = TestList [ testMakeplayersMap, testGetRosterName
                       , testGetRosterYear, testGetPlayerInRoster
                       , testGetPlayerNameInRoster
                       , testGetGiveeInRoster, testSetGiveeInRosterPass
                       , testSetGiveeInRosterFailPlr
                       , testSetGiveeInRosterFailYr, testSetGiveeInRosterFailGv
                       , testSetGiverInRosterPass, testSetGiverInRosterFailPlr
                       , testSetGiverInRosterFailYr, testSetGiverInRosterFailGv
                       , testAddYearInRoster ]

runRosterTests :: IO Counts
runRosterTests = runTestTT $ TestList [ rosterTests ]

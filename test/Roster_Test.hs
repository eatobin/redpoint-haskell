module Roster_Test where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Roster
import           Roster_Utility
import           Test.HUnit

plr1 = Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {giver = "GeoHar", givee = "JohLen"}]}
roster = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]
rosterGE = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "PauMcc"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]
rosterGR = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "RinSta", giver = "GeoHar"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]
rosterAddYear = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"},GiftPair {givee = "none", giver = "none"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"},GiftPair {givee = "none", giver = "none"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"},GiftPair {givee = "none", giver = "none"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"},GiftPair {givee = "none", giver = "none"}]})]
partialRoster = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "none", giver = "RinSta"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "none"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]
testRosterList = [["The Beatles","2014"],["RinSta","Ringo Starr","JohLen","GeoHar"],["JohLen","John Lennon","PauMcc","RinSta"],["GeoHar","George Harrison","RinSta","PauMcc"],["PauMcc","Paul McCartney","GeoHar","JohLen"]]

testGetRosterName = (~=?)
  "The Beatles"
  (getRosterName testRosterList)

testGetRosterYear = (~=?)
  2014
  (getRosterYear testRosterList)

testMakeplayersMap = (~=?)
  roster
  (makePlayersMap testRosterList)

testGetPlayerInRoster = (~=?)
  plr1
  (getPlayerInRoster "RinSta" roster)

testGetPlayerNameInRoster = (~=?)
  "Ringo Starr"
  (getPlayerNameInRoster "RinSta" roster)

testGetGiveeInRoster = (~=?)
  "GeoHar"
  (getGiveeInRoster "PauMcc" roster 0)

testSetGiveeInRosterPass = (~=?)
  rosterGE
  (setGiveeInRoster "GeoHar" 0 "GeoHar" roster)

testSetGiveeInRosterFailPlr = (~=?)
  roster
  (setGiveeInRoster "GeoHarX" 0 "GeoHar" roster)

testSetGiveeInRosterFailYr = (~=?)
  roster
  (setGiveeInRoster "GeoHar" 9 "GeoHar" roster)

testSetGiveeInRosterFailGv = (~=?)
  roster
  (setGiveeInRoster "GeoHar" 0 "GeoHarX" roster)

testSetGiverInRosterPass = (~=?)
  rosterGR
  (setGiverInRoster "GeoHar" 0 "GeoHar" roster)

testSetGiverInRosterFailPlr = (~=?)
  roster
  (setGiverInRoster "GeoHarX" 0 "GeoHar" roster)

testSetGiverInRosterFailYr = (~=?)
  roster
  (setGiverInRoster "GeoHar" 9 "GeoHar" roster)

testSetGiverInRosterFailGv = (~=?)
  roster
  (setGiverInRoster "GeoHar" 0 "GeoHarX" roster)

testAddYearInRoster = (~=?)
  rosterAddYear
  (addYearInRoster roster)

rosterTests = TestList [ testMakeplayersMap, testGetRosterName
                       , testGetRosterYear, testGetPlayerInRoster
                       , testGetPlayerNameInRoster
                       , testGetGiveeInRoster, testSetGiveeInRosterPass
                       , testSetGiveeInRosterFailPlr
                       , testSetGiveeInRosterFailYr, testSetGiveeInRosterFailGv
                       , testSetGiverInRosterPass, testSetGiverInRosterFailPlr
                       , testSetGiverInRosterFailYr, testSetGiverInRosterFailGv
                       , testAddYearInRoster ]

runRosterTests = runTestTT $ TestList [ rosterTests ]

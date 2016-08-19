module Roster_Test where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Roster
import           Roster_Utility
import           Test.HUnit

plr1 = Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {giver = "GeoHar", givee = "JohLen"}]}
-- plr2 = Player {pName = "John Lennon", giftHist = [GiftPair {giver = "RinSta", givee = "PauMcc"}]}
-- plr3 = Player {pName = "George Harrison", giftHist = [GiftPair {giver = "PauMcc", givee = "RinSta"}]}
-- plr4 = Player {pName = "Paul McCartney", giftHist = [GiftPair {giver = "JohLen", givee = "GeoHar"}]}
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
testRosterString = "The Beatles, 2014\nRinSta, Ringo Starr, JohLen, GeoHar\nJohLen, John Lennon, PauMcc, RinSta\nGeoHar, George Harrison, RinSta, PauMcc\nPauMcc, Paul McCartney, GeoHar, JohLen"
rosterAddYear = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"},GiftPair {givee = "none", giver = "none"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"},GiftPair {givee = "none", giver = "none"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"},GiftPair {givee = "none", giver = "none"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"},GiftPair {givee = "none", giver = "none"}]})]
partialString = "The Partial Beatles, 2014\nRinSta, Ringo Starr, JohLen, GeoHar\nJohLen, John Lennon, none, RinSta\nGeoHar, George Harrison, RinSta, PauMcc\nPauMcc, Paul McCartney, GeoHar, none\n"
partialRoster = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "none", giver = "RinSta"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "none"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]
-- rstrLst = [("RinSta", Player {pName = "Ringo Starr", giftHist = [GiftPair {giver = "GeoHar", givee = "JohLen"}]}), ("JohLen", plr2), ("GeoHar", plr3), ("PauMcc", plr4)]
-- rstrMap = Map.fromList rstrLst
-- johLen = rstrMap ! "JohLen"
-- pauMcc = rstrMap ! "PauMcc"
gh0 = Seq.fromList [GiftPair {giver = "GeoHar", givee = "JohLen"}]
gh1 = Seq.fromList [GiftPair {giver = "GeoHarX", givee = "JohLenX"}, GiftPair {giver = "EriTob", givee = "ScoTob"}]
-- fstPr = gh1 !! 0
gp1 = GiftPair {giver = "RinSta", givee = "RinSta"}
-- ["RinSta" "Ringo Starr" "JohLen" "GeoHar"]


recipRoster = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "JohLen"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "GeoHar"}]})]


-- testRosterString = "The Beatles, 2014\nRinSta, Ringo Starr, JohLen, GeoHar\nJohLen, John Lennon, PauMcc, RinSta\nGeoHar, George Harrison, RinSta, PauMcc\nPauMcc, Paul McCartney, GeoHar, JohLen"
testRosterList = [["The Beatles","2014"],["RinSta","Ringo Starr","JohLen","GeoHar"],["JohLen","John Lennon","PauMcc","RinSta"],["GeoHar","George Harrison","RinSta","PauMcc"],["PauMcc","Paul McCartney","GeoHar","JohLen"]]
testRosterLine = ["The Beatles","2014"]
-- roster = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = [GiftPair {givee = "RinSta", giver = "PauMcc"}]})]
makeNewGH = gh0 Seq.|> gp1

testGetRosterName = (~=?)
  "The Beatles"
  (getRosterName testRosterLine)

testGetRosterYear = (~=?)
  2014
  (getRosterYear testRosterLine)

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

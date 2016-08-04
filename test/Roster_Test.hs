module Roster_Test where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Roster
import           Roster_Create
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

-- rstrLst = [("RinSta", Player {pName = "Ringo Starr", giftHist = [GiftPair {giver = "GeoHar", givee = "JohLen"}]}), ("JohLen", plr2), ("GeoHar", plr3), ("PauMcc", plr4)]
-- rstrMap = Map.fromList rstrLst
-- johLen = rstrMap ! "JohLen"
-- pauMcc = rstrMap ! "PauMcc"
gh0 = Seq.fromList [GiftPair {giver = "GeoHar", givee = "JohLen"}]
gh1 = Seq.fromList [GiftPair {giver = "GeoHarX", givee = "JohLenX"}, GiftPair {giver = "EriTob", givee = "ScoTob"}]
-- fstPr = gh1 !! 0

-- ["RinSta" "Ringo Starr" "JohLen" "GeoHar"]

-- testRosterString = "The Beatles, 2014\nRinSta, Ringo Starr, JohLen, GeoHar\nJohLen, John Lennon, PauMcc, RinSta\nGeoHar, George Harrison, RinSta, PauMcc\nPauMcc, Paul McCartney, GeoHar, JohLen"
testRosterList = [["The Beatles","2014"],["RinSta","Ringo Starr","JohLen","GeoHar"],["JohLen","John Lennon","PauMcc","RinSta"],["GeoHar","George Harrison","RinSta","PauMcc"],["PauMcc","Paul McCartney","GeoHar","JohLen"]]
testRosterLine = ["The Beatles","2014"]
-- roster = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = [GiftPair {givee = "RinSta", giver = "PauMcc"}]})]

testGetRosterName = (~=?)
  "The Beatles"
  (getRosterName testRosterLine)

testGetRosterYear = (~=?)
  2014
  (getRosterYear testRosterLine)

testMakeplayersMap = (~=?)
  roster
  (makePlayersMap testRosterList)

testGetPlayer = (~=?)
  plr1
  (getPlayer "RinSta" roster)

testGetPlayerName = (~=?)
  "Ringo Starr"
  (getPlayerName "RinSta" roster)

testGetGivee = (~=?)
  "GeoHar"
  (getGivee "PauMcc" roster 0)

testSetGiveePass = (~=?)
  rosterGE
  (setGivee "GeoHar" 0 "GeoHar" roster)

testSetGiveeFailPlr = (~=?)
  roster
  (setGivee "GeoHarX" 0 "GeoHar" roster)

testSetGiveeFailYr = (~=?)
  roster
  (setGivee "GeoHar" 9 "GeoHar" roster)

testSetGiveeFailGv = (~=?)
  roster
  (setGivee "GeoHar" 0 "GeoHarX" roster)



testSetGiverPass = (~=?)
  rosterGR
  (setGiver "GeoHar" 0 "GeoHar" roster)

testSetGiverFailPlr = (~=?)
  roster
  (setGiver "GeoHarX" 0 "GeoHar" roster)

testSetGiverFailYr = (~=?)
  roster
  (setGiver "GeoHar" 9 "GeoHar" roster)

testSetGiverFailGv = (~=?)
  roster
  (setGiver "GeoHar" 0 "GeoHarX" roster)



rosterTests = TestList [ testMakeplayersMap, testGetRosterName
                       , testGetRosterYear, testGetPlayer, testGetPlayerName
                       , testGetGivee, testSetGiveePass, testSetGiveeFailPlr
                       , testSetGiveeFailYr, testSetGiveeFailGv
                       , testSetGiverPass, testSetGiverFailPlr
                       , testSetGiverFailYr, testSetGiverFailGv ]

runRosterTests = runTestTT $ TestList [ rosterTests ]

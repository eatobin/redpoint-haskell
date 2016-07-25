module Roster_Test where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Roster
import           Test.HUnit
import Roster_Create

plr1 = Player {pName = "Ringo Starr", giftHist = [GiftPair {giver = "GeoHar", givee = "JohLen"}]}
-- plr2 = Player {pName = "John Lennon", giftHist = [GiftPair {giver = "RinSta", givee = "PauMcc"}]}
-- plr3 = Player {pName = "George Harrison", giftHist = [GiftPair {giver = "PauMcc", givee = "RinSta"}]}
-- plr4 = Player {pName = "Paul McCartney", giftHist = [GiftPair {giver = "JohLen", givee = "GeoHar"}]}
playersRoster = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = [GiftPair {givee = "RinSta", giver = "PauMcc"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = [GiftPair {givee = "PauMcc", giver = "RinSta"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = [GiftPair {givee = "GeoHar", giver = "JohLen"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]
rosterString = "The Beatles, 2014\nRinSta, Ringo Starr, JohLen, GeoHar\nJohLen, John Lennon, PauMcc, RinSta\nGeoHar, George Harrison, RinSta, PauMcc\nPauMcc, Paul McCartney, GeoHar, JohLen"

-- rstrLst = [("RinSta", Player {pName = "Ringo Starr", giftHist = [GiftPair {giver = "GeoHar", givee = "JohLen"}]}), ("JohLen", plr2), ("GeoHar", plr3), ("PauMcc", plr4)]
-- rstrMap = Map.fromList rstrLst
-- johLen = rstrMap ! "JohLen"
-- pauMcc = rstrMap ! "PauMcc"
-- gh0 = [GiftPair {giver = "GeoHar", givee = "JohLen"}]
-- gh1 = [GiftPair {giver = "GeoHar", givee = "JohLen"}, GiftPair {giver = "EriTo b", givee = "ScoTob"}]
-- fstPr = gh1 !! 0

-- ["RinSta" "Ringo Starr" "JohLen" "GeoHar"]

-- rosterString = "The Beatles, 2014\nRinSta, Ringo Starr, JohLen, GeoHar\nJohLen, John Lennon, PauMcc, RinSta\nGeoHar, George Harrison, RinSta, PauMcc\nPauMcc, Paul McCartney, GeoHar, JohLen"
-- rosterList = [["The Beatles","2014"],["RinSta","Ringo Starr","JohLen","GeoHar"],["JohLen","John Lennon","PauMcc","RinSta"],["GeoHar","George Harrison","RinSta","PauMcc"],["PauMcc","Paul McCartney","GeoHar","JohLen"]]
-- rosterLine = ["The Beatles","2014"]
-- playersMap = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = [GiftPair {givee = "RinSta", giver = "PauMcc"}]})]

testGetRosterName = (~=?)
  "The Beatles"
  (getRosterName rosterString)

testGetRosterYear = (~=?)
  "2014"
  (getRosterYear rosterString)

testMakeplayersMap = (~=?)
  playersRoster
  (makeplayersMap rosterString)

testGetPlayer = (~=?)
  plr1
  (getPlayer "RinSta" playersRoster)

testGetPlayerName = (~=?)
  "Ringo Starr"
  (getPlayerName "RinSta" playersRoster)

testGetGiveeCode = (~=?)
  "GeoHar"
  (getGiveeCode "PauMcc" 0 playersRoster)


rosterTests = TestList [ testMakeplayersMap, testGetRosterName
                       , testGetRosterYear, testGetPlayer, testGetPlayerName
                       , testGetGiveeCode ]

runRosterTests = runTestTT $ TestList [ rosterTests ]

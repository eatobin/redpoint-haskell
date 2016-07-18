module Roster_Test where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Roster
import           Test.HUnit

-- plr1 = Player {name = "Ringo Starr", giftHist = [GiftPair {giver = "GeoHar", givee = "JohLen"}]}
-- plr2 = Player {name = "John Lennon", giftHist = [GiftPair {giver = "RinSta", givee = "PauMcc"}]}
-- plr3 = Player {name = "George Harrison", giftHist = [GiftPair {giver = "PauMcc", givee = "RinSta"}]}
-- plr4 = Player {name = "Paul McCartney", giftHist = [GiftPair {giver = "JohLen", givee = "GeoHar"}]}
playersString = "RinSta, Ringo Starr, JohLen, GeoHar\nJohLen, John Lennon, PauMcc, RinSta\nGeoHar, George Harrison, RinSta, PauMcc\nPauMcc, Paul McCartney, GeoHar, JohLen"
playersRoster = Map.fromList [("GeoHar",Player {name = "George Harrison", giftHist = [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),("JohLen",Player {name = "John Lennon", giftHist = [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),("PauMcc",Player {name = "Paul McCartney", giftHist = [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),("RinSta",Player {name = "Ringo Starr", giftHist = [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]

-- rstrLst = [("RinSta", Player {name = "Ringo Starr", giftHist = [GiftPair {giver = "GeoHar", givee = "JohLen"}]}), ("JohLen", plr2), ("GeoHar", plr3), ("PauMcc", plr4)]
-- rstrMap = Map.fromList rstrLst
-- johLen = rstrMap ! "JohLen"
-- pauMcc = rstrMap ! "PauMcc"
-- gh0 = [GiftPair {giver = "GeoHar", givee = "JohLen"}]
-- gh1 = [GiftPair {giver = "GeoHar", givee = "JohLen"}, GiftPair {giver = "EriTob", givee = "ScoTob"}]
-- fstPr = gh1 !! 0

-- ["RinSta" "Ringo Starr" "JohLen" "GeoHar"]
--
testMakeRoster = (~=?)
  playersRoster
  (makeRoster playersString)
--
-- testGetName = (~=?)
--   "Borrower1"
--   (getName br1)
--
-- testSetName = (~=?)
--   br1
--   (setName "Borrower1" (Borrower "Jack" 1))
--
-- testGetMaxBooks = (~=?)
--   1
--   (getMaxBooks br1)
--
-- testSetMaxBooks = (~=?)
--   Borrower {name = "Borrower1", maxBooks = 11}
--   (setMaxBooks 11 br1)
--
-- testBorrowerToString = (~=?)
--   "Borrower1 (1 books)"
--   (borrowerToString br1)
--
rosterTests = TestList [ testMakeRoster ]
runRosterTests = runTestTT $ TestList [ rosterTests ]

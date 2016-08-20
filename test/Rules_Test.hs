module Rules_Test where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Hat
import           Roster
import           Roster_Test
import           Roster_Utility
import           Rules
import           Test.HUnit

recipRoster = Map.fromList [("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "JohLen"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "GeoHar"}]})]
rosterP0 = Map.fromList [("EriTob",Player {pName = "Eric Tobin", giftHist = Seq.fromList [GiftPair {givee = "KarLav", giver = "PauMcc"}]})
  ,("GeoHar",Player {pName = "George Harrison", giftHist = Seq.fromList [GiftPair {givee = "PauMcc", giver = "JohLen"}]})
  ,("JohLen",Player {pName = "John Lennon", giftHist = Seq.fromList [GiftPair {givee = "GeoHar", giver = "RinSta"}]})
  ,("KarLav",Player {pName = "Karen Lavengood", giftHist = Seq.fromList [GiftPair {givee = "RinSta", giver = "EriTob"}]})
  ,("PauMcc",Player {pName = "Paul McCartney", giftHist = Seq.fromList [GiftPair {givee = "EriTob", giver = "GeoHar"}]})
  ,("RinSta",Player {pName = "Ringo Starr", giftHist = Seq.fromList [GiftPair {givee = "JohLen", giver = "KarLav"}]})]

rosterP4 =
  let
    extended = addYearInRoster $ addYearInRoster $ addYearInRoster $ addYearInRoster rosterP0
  in
    setGiveeInRoster "RinSta" 4 "KarLav" $ setGiveeInRoster "RinSta" 3 "EriTob" $ setGiveeInRoster "RinSta" 2 "PauMcc" $ setGiveeInRoster "RinSta" 1 "GeoHar" extended

testGiveeNotSelf = (~=?)
  True
  (giveeNotSelf "JohLen" "GeoHar")

testGiveeNotRecipPass = (~=?)
  True
  (giveeNotRecip "JohLen" "GeoHar" 0 roster)

testGiveeNotRecipFail = (~=?)
  False
  (giveeNotRecip "JohLen" "GeoHar" 0 recipRoster)

testGiveeNotRepeatFail1 = (~=?)
  False
  (giveeNotRepeat "RinSta" "JohLen" 2 rosterP4)

testGiveeNotRepeatFail2 = (~=?)
  False
  (giveeNotRepeat "RinSta" "GeoHar" 2 rosterP4)

testGiveeNotRepeatPass3 = (~=?)
  True
  (giveeNotRepeat "RinSta" "KarLav" 2 rosterP4)

testGiveeNotRepeatPass4 = (~=?)
  True
  (giveeNotRepeat "RinSta" "JohLen" 5 rosterP4)

testGiveeNotRepeatPass5 = (~=?)
  True
  (giveeNotRepeat "RinSta" "GeoHar" 5 rosterP4)

testGiveeNotRepeatFail6 = (~=?)
  False
  (giveeNotRepeat "RinSta" "PauMcc" 5 rosterP4)

testGiveeNotRepeatFail7 = (~=?)
  False
  (giveeNotRepeat "RinSta" "EriTob" 5 rosterP4)

testGiveeNotRepeatFail8 = (~=?)
  False
  (giveeNotRepeat "RinSta" "KarLav" 5 rosterP4)

rulesTests = TestList [ testGiveeNotSelf, testGiveeNotRecipPass
                      , testGiveeNotRecipFail, testGiveeNotRepeatFail1
                      , testGiveeNotRepeatFail2, testGiveeNotRepeatPass3
                      , testGiveeNotRepeatPass4, testGiveeNotRepeatPass5
                      , testGiveeNotRepeatFail6, testGiveeNotRepeatFail7
                      , testGiveeNotRepeatFail8 ]

runRulesTests = runTestTT $ TestList [ rulesTests ]

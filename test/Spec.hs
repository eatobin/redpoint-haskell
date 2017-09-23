{-# OPTIONS -Wall #-}

import           Test.Hspec

import           Roster_Utility

bs :: String
bs = "The Beatles, 2014\nRinSta, Ringo Starr, JohLen, GeoHar\nJohLen, John Lennon, PauMcc, RinSta\nGeoHar, George Harrison, RinSta, PauMcc\nPauMcc, Paul McCartney, GeoHar, JohLen"

rl :: RosterList
rl = [["The Beatles","2014"],["RinSta","Ringo Starr","JohLen","GeoHar"],["JohLen","John Lennon","PauMcc","RinSta"],["GeoHar","George Harrison","RinSta","PauMcc"],["PauMcc","Paul McCartney","GeoHar","JohLen"]]

pl :: RosterList
pl = [["RinSta","Ringo Starr","JohLen","GeoHar"],["JohLen","John Lennon","PauMcc","RinSta"],["GeoHar","George Harrison","RinSta","PauMcc"],["PauMcc","Paul McCartney","GeoHar","JohLen"]]

main :: IO ()
main = hspec $
  describe "Roster_Utility tests" $ do
    it "testmakeRosterList" $
      makeRosterList bs `shouldBe` rl

    it "testmakeRosterInfo" $
      makeRosterInfo rl `shouldBe` ["The Beatles","2014"]

    it "testmakePlayersList" $
      makePlayersList rl `shouldBe` pl

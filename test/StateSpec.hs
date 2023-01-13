module StateSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Gift_Pair
import Hat
import Player
import Players
import Roster
import State
import Test.Hspec

testHat :: Hat
testHat = Set.fromList ["RinSta"]

--jsonString :: JsonString
--jsonString = "{\"rosterName\":\"The Beatles\",\"rosterYear\":2014,\"players\":{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}}"
--
--rosterPlayers :: Players
--rosterPlayers =
--  Map.fromList
--    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
--      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
--      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
--      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
--    ]
--
--roster :: Roster
--roster = Roster {rosterName = "The Beatles", rosterYear = 2014, players = rosterPlayers}

spec :: Spec
spec = do
  describe "stateDrawPuck" $ do
    it "should draw a puck" $ stateDrawPuck testHat `shouldReturn` Just "RinSta"
    it "should NOT draw a puck" $ stateDrawPuck Set.empty `shouldReturn` Nothing

--  describe "rosterYear" $ do
--    it "should return 2014 rosterYear" $ rosterYear roster `shouldBe` 2014
--
--  describe "rosterJsonStringToRoster" $ do
--    it "should convert from JSON" $ rosterJsonStringToRoster jsonString `shouldBe` Just roster

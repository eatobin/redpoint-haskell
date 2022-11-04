module HelpersSpec (spec) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Gift_Pair
import Helpers
import Player
import Players
import Test.Hspec

players1 :: Players
players1 =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

spec :: Spec
spec = do
  describe "helpersReadFileIntoJsonString - PASS" $ do
    it "given a valid filepath, returns a Right JsonString" $
      helpersReadFileIntoJsonString "resources-test/bad-json.json"
        `shouldReturn` Just "[ \"test\" :: 123 ]\n"

  describe "helpersReadFileIntoJsonString - FAIL" $ do
    it "given an invalid filepath, returns a Left ErrorString" $
      helpersReadFileIntoJsonString "resources-test/no-file.json"
        `shouldReturn` Nothing

  describe "helpersRosterOrQuit" $ do
    it "given a valid filepath and TVarPlayers, returns an IO (RosterName, RosterYear)\n    and sets the TVarPlayers" $ do
      playersTVarPlayers <- STM.atomically (STM.newTVar (Map.empty :: Players))
      ioPair <- helpersRosterOrQuit "resources-test/beatles.json" playersTVarPlayers
      plrs <- STM.readTVarIO playersTVarPlayers
      ioPair `shouldBe` ("The Beatles", 2014)
      plrs `shouldBe` players1

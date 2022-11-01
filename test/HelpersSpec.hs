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
  describe "helpersRosterOrQuit" $ do
    it "given a valid filepath and TVarPlayers,\n    returns an IO (RosterName, RosterYear)\n    and sets the TVarPlayers\n    or Quits with an ErrorString" $ do
      playersTVarPlayers <- STM.atomically (STM.newTVar (Map.empty :: Players))
      ioPair <- helpersRosterOrQuit "resources-test/beatles.json" playersTVarPlayers
      plrs <- STM.readTVarIO playersTVarPlayers
      ioPair `shouldBe` ("The Beatles", 2014)
      plrs `shouldBe` players1
      helpersReadFileIntoJsonString "resources-test/no-file.json"
        `shouldReturn` Left "file read error."

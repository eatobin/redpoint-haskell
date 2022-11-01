module HelpersSpec (spec) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Gift_Pair
import Helpers
import Player
import Players
import Test.Hspec

--fromAcctSTMTVarInt :: STM.STM (STM.TVar Int)
--fromAcctSTMTVarInt = STM.newTVar (375 :: Int)

--emptyPlayersSTMTVarInt :: STM.TVar Players
emptyPlayersSTMTVarPlayers :: STM.STM (STM.TVar Players)
--emptyPlayersSTMTVarInt :: IO (STM.TVar Players)
emptyPlayersSTMTVarPlayers = STM.newTVar (emptyPlayers :: Players)

--emptyPlayersTVarInt <- STM.atomically emptyPlayersSTMTVarInt

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
        `shouldReturn` Right "[ \"test\" :: 123 ]\n"

  describe "helpersReadFileIntoJsonString - FAIL" $ do
    it "given an invalid filepath, returns a Left ErrorString" $
      helpersReadFileIntoJsonString "resources-test/no-file.json"
        `shouldReturn` Left "file read error."

  describe "helpersRosterOrQuit - PASS-1" $ do
    it "given a valid filepath and TVarPlayers, returns an IO (RosterName, RosterYear)" $ do
      emptyPlayersTVarInt <- STM.atomically emptyPlayersSTMTVarPlayers
      helpersRosterOrQuit "resources-test/beatles.json" emptyPlayersTVarInt
        `shouldReturn` ("The Beatles", 2014)

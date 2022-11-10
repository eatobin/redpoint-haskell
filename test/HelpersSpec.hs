module HelpersSpec (spec) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
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

players2 :: Players
players2 =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}, GiftPair {givee = "GeoHar", giver = "GeoHar"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}, GiftPair {givee = "JohLen", giver = "JohLen"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "PauMcc", giver = "PauMcc"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}, GiftPair {givee = "RinSta", giver = "RinSta"}]})
    ]

spec :: Spec
spec = do
  describe "helpersReadFileIntoJsonString - PASS" $ do
    it "should return a Right JsonString" $
      helpersReadFileIntoJsonString "resources-test/bad-json.json"
        `shouldReturn` Right "[ \"test\" :: 123 ]\n"

  describe "helpersReadFileIntoJsonString - FAIL" $ do
    it "should return a  Left ErrorString" $
      helpersReadFileIntoJsonString "resources-test/no-file.json"
        `shouldReturn` Left "file read error"

    describe "helpersRosterOrQuit" $ do
      it "given a valid filepath and TVarPlayers, returns an IO (RosterName, RosterYear)\n    and sets the TVarPlayers" $ do
        tVarRosterName <- STM.atomically (STM.newTVar "")
        tVarRosterYear <- STM.atomically (STM.newTVar (0 :: Int))
        tVarPlayers <- STM.atomically (STM.newTVar (Map.empty :: Players))
        helpersRosterOrQuit "resources-test/beatles.json" tVarRosterName tVarRosterYear tVarPlayers
        rn <- STM.readTVarIO tVarRosterName
        ry <- STM.readTVarIO tVarRosterYear
        plrs <- STM.readTVarIO tVarPlayers
        rn `shouldBe` "The Beatles"
        ry `shouldBe` 2014
        plrs `shouldBe` players1

  describe "helpersStartNewYear" $ do
    it "resets TVarGiftYear, TVarPlayers, TVarGiverHat, TVarGiveeHat,\n    TVarMaybeGiver, TVarMaybeGivee and TVarDiscards" $ do
      tVarRosterName <- STM.atomically (STM.newTVar "")
      tVarRosterYear <- STM.atomically (STM.newTVar (0 :: Int))
      tVarPlayers <- STM.atomically (STM.newTVar (Map.empty :: Players))
      helpersRosterOrQuit "resources-test/beatles.json" tVarRosterName tVarRosterYear tVarPlayers
      tVarGiftYear <- STM.atomically (STM.newTVar 0)
      tVarGiverHat <- STM.atomically (STM.newTVar Set.empty)
      tVarGiveeHat <- STM.atomically (STM.newTVar Set.empty)
      tVarMaybeGiver <- STM.atomically (STM.newTVar Nothing)
      tVarMaybeGivee <- STM.atomically (STM.newTVar Nothing)
      tVarDiscards <- STM.atomically (STM.newTVar Set.empty)
      helpersStartNewYear tVarGiftYear tVarPlayers tVarGiverHat tVarGiveeHat tVarMaybeGiver tVarMaybeGivee tVarDiscards
      gyX <- STM.readTVarIO tVarGiftYear
      gyX `shouldBe` 1
      plrs <- STM.readTVarIO tVarPlayers
      plrs `shouldBe` players2
      grh <- STM.readTVarIO tVarGiverHat
      grh `shouldBe` Set.fromList ["GeoHar", "JohLen", "PauMcc", "RinSta"]
      geh <- STM.readTVarIO tVarGiveeHat
      geh `shouldBe` Set.fromList ["GeoHar", "JohLen", "PauMcc", "RinSta"]
      mgrX <- STM.readTVarIO tVarMaybeGiver
      mgrX `shouldNotBe` Nothing
      mgeX <- STM.readTVarIO tVarMaybeGivee
      mgeX `shouldNotBe` Nothing
      dis <- STM.readTVarIO tVarDiscards
      dis `shouldBe` Set.empty

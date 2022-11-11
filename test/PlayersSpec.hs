module PlayersSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Gift_Pair
import Player
import Players
import Test.Hspec

jsonString :: JsonString
jsonString = "{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}"

players :: Players
players =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

newBeePlayers :: Players
newBeePlayers =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "New Bee", giftHistory = Seq.fromList [GiftPair {givee = "NewBee", giver = "NewBee"}]})
    ]

playersExt :: Players
playersExt =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}, GiftPair {givee = "GeoHar", giver = "GeoHar"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}, GiftPair {givee = "JohLen", giver = "JohLen"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "PauMcc", giver = "PauMcc"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}, GiftPair {givee = "RinSta", giver = "RinSta"}]})
    ]

spec :: Spec
spec = do
  describe "playersUpdatePlayer" $ do
    it "should return an updated player" $ playersUpdatePlayer "RinSta" Player {playerName = "New Bee", giftHistory = Seq.fromList [GiftPair {giver = "NewBee", givee = "NewBee"}]} players `shouldBe` newBeePlayers

  describe "playersGetPlayerName" $ do
    it "should return a player name" $ playersGetPlayerName "PauMcc" players `shouldBe` "Paul McCartney"

  describe "playersAddYear" $ do
    it "should add a new year" $ playersAddYear players `shouldBe` playersExt

  describe "playersJsonStringToPlayers" $ do
    it "should convert from JSON" $ playersJsonStringToPlayers jsonString `shouldBe` Just players

  describe "playersGetMyGivee and playersGetMyGiver" $ do
    it "should return a givee" $ playersGetMyGivee "JohLen" players 0 `shouldBe` "PauMcc"
    it "should return a giver" $ playersGetMyGiver "JohLen" players 0 `shouldBe` "RinSta"

--
--  it should "return a givee and a giver" in {
--    assert(playersGetMyGivee("GeoHar")(0)(players) == "RinSta")
--    assert(playersGetMyGiver("GeoHar")(0)(players) == "PauMcc")
--  }
--
--  it should "update a givee and a giver" in {
--    assert(playersUpdateMyGivee("GeoHar")(0)("you")(players) == playersGivee)
--    assert(playersUpdateMyGiver("GeoHar")(0)("you")(players) == playersGiver)

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

spec :: Spec
spec = do
  describe "playersUpdatePlayer" $ do
    it "should return an updated player" $ playersUpdatePlayer "RinSta" Player {playerName = "New Bee", giftHistory = Seq.fromList [GiftPair {giver = "NewBee", givee = "NewBee"}]} players `shouldBe` newBeePlayers

  describe "playersGetPlayerName" $ do
    it "should return a player name" $ playersGetPlayerName "PauMcc" players `shouldBe` "Paul McCartney"

  describe "playersJsonStringToPlayers" $ do
    it "should convert from JSON" $ playersJsonStringToPlayers jsonString `shouldBe` Just players

--it should "return a player name" in {
--    assert(playersGetPlayerName("PauMcc")(players) == "Paul McCartney")
--  }
--
--  it should "add a new year" in {
--    assert(playersAddYear(players) == playersExt)
--  }
--
--  it should "return a givee and a giver" in {
--    assert(playersGetMyGivee("GeoHar")(0)(players) == "RinSta")
--    assert(playersGetMyGiver("GeoHar")(0)(players) == "PauMcc")
--  }
--
--  it should "update a givee and a giver" in {
--    assert(playersUpdateMyGivee("GeoHar")(0)("you")(players) == playersGivee)
--    assert(playersUpdateMyGiver("GeoHar")(0)("you")(players) == playersGiver)

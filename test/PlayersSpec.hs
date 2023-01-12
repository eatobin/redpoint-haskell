module PlayersSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import Gift_Pair
import Player
import Players
import Test.Hspec

jsonString :: JsonString
jsonString = "{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}"

playerGeoHar :: Player
playerGeoHar = Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}

players :: Players
players =
  Map.fromList
    [ ("GeoHar", playerGeoHar),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

newBeePlayers :: Players
newBeePlayers =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "New Bee", giftHistory = Vec.fromList [GiftPair {givee = "NewBee", giver = "NewBee"}]})
    ]

playersExt :: Players
playersExt =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}, GiftPair {givee = "GeoHar", giver = "GeoHar"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}, GiftPair {givee = "JohLen", giver = "JohLen"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "PauMcc", giver = "PauMcc"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}, GiftPair {givee = "RinSta", giver = "RinSta"}]})
    ]

playersGivee :: Players
playersGivee =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "you", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

playersGiver :: Players
playersGiver =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "you"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

spec :: Spec
spec = do
  describe "playersUpdatePlayer" $ do
    it "should return an updated player" $ playersUpdatePlayer "RinSta" Player {playerName = "New Bee", giftHistory = Vec.fromList [GiftPair {giver = "NewBee", givee = "NewBee"}]} players `shouldBe` newBeePlayers

  describe "playersGetPlayerName" $ do
    it "should return a player name" $ playersGetPlayerName "PauMcc" players `shouldBe` "Paul McCartney"

  describe "playersAddYear" $ do
    it "should add a new year" $ playersAddYear players `shouldBe` playersExt

  describe "playersGetMyGivee and playersGetMyGiver" $ do
    it "should return a givee" $ playersGetMyGivee "JohLen" players 0 `shouldBe` "PauMcc"
    it "should return a giver" $ playersGetMyGiver "JohLen" players 0 `shouldBe` "RinSta"

    describe "playersUpdateMyGivee and playersUpdateMyGiver" $ do
      it "should update a givee" $ playersUpdateMyGivee "GeoHar" "you" 0 players `shouldBe` playersGivee
      it "should update a giver" $ playersUpdateMyGiver "GeoHar" "you" 0 players `shouldBe` playersGiver

  describe "playersJsonStringToPlayers" $ do
    it "should convert from JSON" $ playersJsonStringToPlayers jsonString `shouldBe` Just players

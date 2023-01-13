module RosterSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import Gift_Pair
import Player
import Players
import Roster
import Test.Hspec

jsonString :: JsonString
jsonString = "{\"rosterName\":\"The Beatles\",\"rosterYear\":2014,\"players\":{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}}"

rosterPlayers :: Players
rosterPlayers =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

roster :: Roster
roster = Roster {rosterName = "The Beatles", rosterYear = 2014, players = rosterPlayers}

spec :: Spec
spec = do
  --  describe "playersUpdatePlayer" $ do
  --    it "should return an updated player" $ playersUpdatePlayer "RinSta" Player {playerName = "New Bee", giftHistory = Vec.fromList [GiftPair {giver = "NewBee", givee = "NewBee"}]} players `shouldBe` newBeePlayers
  --
  --  describe "playersGetPlayerName" $ do
  --    it "should return a player name" $ playersGetPlayerName "PauMcc" players `shouldBe` "Paul McCartney"
  --
  --  describe "playersAddYear" $ do
  --    it "should add a new year" $ playersAddYear players `shouldBe` playersExt
  --
  --  describe "playersGetMyGivee and playersGetMyGiver" $ do
  --    it "should return a givee" $ playersGetMyGivee "JohLen" players 0 `shouldBe` "PauMcc"
  --    it "should return a giver" $ playersGetMyGiver "JohLen" players 0 `shouldBe` "RinSta"
  --
  --    describe "playersUpdateMyGivee and playersUpdateMyGiver" $ do
  --      it "should update a givee" $ playersUpdateMyGivee "GeoHar" "you" 0 players `shouldBe` playersGivee
  --      it "should update a giver" $ playersUpdateMyGiver "GeoHar" "you" 0 players `shouldBe` playersGiver

  describe "rosterJsonStringToRoster" $ do
    it "should convert from JSON" $ rosterJsonStringToRoster jsonString `shouldBe` Just roster

--package com.eatobin.redpointscala
--
--import com.eatobin.redpointscala.GiftPair.JsonString
--import com.eatobin.redpointscala.Roster.ErrorString
--import org.scalatest.flatspec.AnyFlatSpec
--
--class RosterSpec extends AnyFlatSpec {
--
--  private val jsonStringRos: JsonString = "{\"rosterName\":\"The Beatles\",\"rosterYear\":2014,\"players\":{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}}"
--
--  private val rinSta: Player = Player("Ringo Starr", Vector(GiftPair("JohLen", "GeoHar")))
--  private val johLen: Player = Player("John Lennon", Vector(GiftPair("PauMcc", "RinSta")))
--  private val geoHar: Player = Player("George Harrison", Vector(GiftPair("RinSta", "PauMcc")))
--  private val pauMcc: Player = Player("Paul McCartney", Vector(GiftPair("GeoHar", "JohLen")))
--  private val players: Map[String, Player] =
--    Map("RinSta" -> rinSta, "JohLen" -> johLen, "GeoHar" -> geoHar, "PauMcc" -> pauMcc)
--  private val roster: Roster = Roster("The Beatles", 2014, players)
--
--  private val jsBeatlesBad: JsonString = "{\"rosterName\"\"The Beatles\",\"rosterYear\":2014,\"players\":{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}}"
--
--  "A Roster" should "return \"The Beatles\" rosterName" in {
--    assert(roster.rosterName == "The Beatles")
--  }
--
--  it should "return 2014 rosterYear" in {
--    assert(roster.rosterYear == 2014)
--  }
--
--  it should "convert from JSON - or not" in {
--    val rosJson: Either[ErrorString, Roster] = Roster.rosterJsonStringToRoster(Right(jsonStringRos))
--    val rosJsonBad: Either[ErrorString, Roster] = Roster.rosterJsonStringToRoster(Right(jsBeatlesBad))
--    val rosNoFile: Either[ErrorString, Roster] = Roster.rosterJsonStringToRoster(Left("Just made this up"))
--    assert(rosJson == Right(roster))
--    assert(rosJsonBad == Left("""io.circe.ParsingFailure: expected : got '"The B...' (line 1, column 14)"""))
--    assert(rosNoFile == Left("Just made this up"))
--  }
--}

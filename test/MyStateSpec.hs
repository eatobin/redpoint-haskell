module MyStateSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Gift_Pair
import Hat
import MyState
import Player
import Players
import Test.Hspec

testHat :: Hat
testHat = Set.fromList ["RinSta"]

players0 :: Players
players0 =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

players1 :: Players
players1 =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}, GiftPair {givee = "GeoHar", giver = "GeoHar"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}, GiftPair {givee = "JohLen", giver = "JohLen"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "PauMcc", giver = "PauMcc"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}, GiftPair {givee = "RinSta", giver = "RinSta"}]})
    ]

beatlesState0 :: MyState
beatlesState0 =
  MyState
    { rosterName = "The Beatles",
      rosterYear = 2014,
      players = players0,
      giftYear = 0,
      giveeHat = Set.empty,
      giverHat = Set.empty,
      maybeGivee = Nothing,
      maybeGiver = Nothing,
      discards = Set.empty,
      quit = "n"
    }

beatlesState1 :: MyState
beatlesState1 =
  MyState
    { rosterName = "The Beatles",
      rosterYear = 2014,
      players = players1,
      giftYear = 1,
      giveeHat = Set.fromList ["GeoHar", "JohLen", "PauMcc", "RinSta"],
      giverHat = Set.fromList ["GeoHar", "JohLen", "PauMcc", "RinSta"],
      maybeGivee = Just "GeoHar",
      maybeGiver = Just "PauMcc",
      discards = Set.empty,
      quit = "n"
    }

spec :: Spec
spec = do
  describe "MyState tests" $ do
    simple
    complex

simple :: Spec
simple = do
  describe "myStateDrawPuck" $ do
    it "should draw a puck from a hat" $ myStateDrawPuck testHat `shouldReturn` Just "RinSta"
    it "should NOT draw a puck from an empty hat" $ myStateDrawPuck Set.empty `shouldReturn` Nothing

complex :: Spec
complex = beforeAll (myStateStartNewYear (return beatlesState0)) $ do
  describe "myStateStartNewYear" $ do
    it "should update the players" $ \bs -> do
      players bs `shouldBe` players1
    it "check the year" $ \bs -> do
      rosterYear bs `shouldBe` 2014

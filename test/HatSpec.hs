module HatSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Gift_Pair
import Hat
import Player
import Players
import Test.Hspec

players1 :: Players
players1 =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

hat1 :: Hat
hat1 = Set.fromList ["RinSta", "GeoHar", "JohLen", "PauMcc", "JohLen"]

hat2 :: Hat
hat2 = Set.fromList ["RinSta", "GeoHar", "PauMcc"]

discards :: Hat
discards = Set.fromList ["JohLen"]

spec :: Spec
spec = do
  describe "hatMakeHat" $ do
    it "should make itself given players" $ hatMakeHat players1 `shouldBe` hat1
  describe "hatRemovePuck" $ do
    it "should remove a puck" $ hatRemovePuck "JohLen" hat1 `shouldBe` hat2
  describe "hatDiscardGivee" $ do
    it "should discard a puck" $ hatDiscardGivee "JohLen" hat2 `shouldBe` hat1
  describe "hatReturnDiscards" $ do
    it "should return discarded givees" $ hatReturnDiscards discards hat2 `shouldBe` hat1

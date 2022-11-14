module HatSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Gift_Pair
import Hat
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

--  it "testHatMakeHat" $ hatMakeHat players1 `shouldBe` hat1
--  it "testHatRemovePuck" $ hatRemovePuck "JohLen" hat1 `shouldBe` hat2
--  it "testHatDiscardGivee" $ hatDiscardGivee "JohLen" hat2 `shouldBe` hat1
--  it "testHatReturnDiscards" $ hatReturnDiscards discards hat2 `shouldBe` hat1

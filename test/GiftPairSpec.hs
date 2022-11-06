module GiftPairSpec (spec) where

import Gift_Pair
import Test.Hspec

jsonString :: JsonString
jsonString = "{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}"

giftPair :: GiftPair
giftPair = GiftPair {givee = "GeoHar", giver = "JohLen"}

spec :: Spec
spec = do
  describe "giftPairUpdateGivee" $ do
    it "should update a givee" $ giftPairUpdateGivee "NewBee" giftPair `shouldBe` GiftPair "NewBee" "JohLen"
  describe "giftPairUpdateGiver" $ do
    it "should update a giver" $ giftPairUpdateGiver "NewBee" giftPair `shouldBe` GiftPair "GeoHar" "NewBee"
  describe "giftPairJsonStringToGiftPair" $ do
    it "convert from JSON" $ giftPairJsonStringToGiftPair jsonString `shouldBe` Just (GiftPair "GeoHar" "JohLen")

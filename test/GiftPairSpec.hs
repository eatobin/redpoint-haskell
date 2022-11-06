module GiftPairSpec (spec) where

import Gift_Pair
import Test.Hspec

jsonString :: JsonString
jsonString = "{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}"

giftPair :: GiftPair
giftPair = GiftPair {giver = "JohLen", givee = "GeoHar"}

spec :: Spec
spec = do
  describe "giftPairUpdateGivee" $ do
    it "should update a givee" $ giftPairUpdateGivee "NewBee" giftPair `shouldBe` GiftPair "NewBee" "JohLen"
  describe "giftPairUpdateGiver" $ do
    it "should update a giver" $ giftPairUpdateGiver "NewBee" giftPair `shouldBe` GiftPair "GeoHar" "NewBee"
  describe "giftPairJsonStringToGiftPair" $ do
    it "convert from JSON" $ giftPairJsonStringToGiftPair jsonString `shouldBe` Just giftPair
  describe "giftPairGiftPairToJsonString" $ do
    it "convert to JSON" $ giftPairGiftPairToJsonString giftPair `shouldBe` jsonString

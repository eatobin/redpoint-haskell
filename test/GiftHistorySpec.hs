module GiftHistorySpec (spec) where

import qualified Data.Sequence as Seq
import Gift_History
import Gift_Pair
import Test.Hspec

jsonString :: JsonString
jsonString = "[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]"

giftHistory :: GiftHistory
giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]

spec :: Spec
spec = do
  describe "giftHistoryAddYear" $ do
    it "should add a new year" $ giftHistoryAddYear "NewBee" giftHistory `shouldBe` Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "NewBee", giver = "NewBee"}]
  describe "giftHistoryUpdateGiftHistory" $ do
    it "should return an updated giftHistory" $ giftHistoryUpdateGiftHistory 0 (GiftPair "me" "you") giftHistory `shouldBe` Seq.fromList [GiftPair {givee = "me", giver = "you"}]
  describe "giftPairJsonStringToGiftPair" $ do
    it "should convert from JSON" $ giftHistoryJsonStringToGiftHistory jsonString `shouldBe` Just giftHistory

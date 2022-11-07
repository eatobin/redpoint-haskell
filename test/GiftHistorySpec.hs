module GiftHistorySpec (spec) where

import qualified Data.Sequence as Seq
import Gift_History
import Gift_Pair
import Test.Hspec

jsonString :: JsonString
jsonString = "[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]"

giftHistory1 :: GiftHistory
giftHistory1 = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]

giftHistory2 :: GiftHistory
giftHistory2 = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "Yippee", giver = "Yippee"}]

giftHistory3 :: GiftHistory
giftHistory3 = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "Givee1", giver = "Giver1"}]

spec :: Spec
spec = do
  describe "giftHistoryAddYear" $ do
    it "should add a new year" $ giftHistoryAddYear "Yippee" giftHistory1 `shouldBe` giftHistory2

--  describe "giftPairUpdateGiver" $ do
--    it "should update a giver" $ giftPairUpdateGiver "NewBee" giftPair `shouldBe` GiftPair "GeoHar" "NewBee"
--  describe "giftPairJsonStringToGiftPair" $ do
--    it "should convert from JSON" $ giftPairJsonStringToGiftPair jsonString `shouldBe` Just giftPair
--  describe "giftPairGiftPairToJsonString" $ do
--    it "should convert to JSON" $ giftPairGiftPairToJsonString giftPair `shouldBe` jsonString
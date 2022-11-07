module PlayerSpec (spec) where

import qualified Data.Sequence as Seq
import Gift_History
import Gift_Pair
import Player
import Test.Hspec

jsonString :: JsonString
jsonString = "{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]}"

player :: Player
player = Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}

spec :: Spec
spec = do
  describe "playerUpdateGiftHistory" $ do
    it "should return an updated giftHistory" $ playerUpdateGiftHistory (Seq.fromList [GiftPair {givee = "nope", giver = "yup"}]) player
      `shouldBe` Player "Paul McCartney" (Seq.fromList [GiftPair "nope" "yup"])

--  describe "giftHistoryUpdateGiftHistory" $ do
--    it "should return an updated giftHistory" $ giftHistoryUpdateGiftHistory 0 (GiftPair "me" "you") giftHistory `shouldBe` Seq.fromList [GiftPair {givee = "me", giver = "you"}]
--  describe "giftPairJsonStringToGiftPair" $ do
--    it "should convert from JSON" $ giftHistoryJsonStringToGiftHistory jsonString `shouldBe` Just giftHistory
--  describe "giftPairGiftPairToJsonString" $ do
--    it "should convert to JSON" $ giftHistoryGiftHistoryToJsonString giftHistory `shouldBe` jsonString

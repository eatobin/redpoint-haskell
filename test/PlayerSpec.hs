module PlayerSpec (spec) where

import qualified Data.Sequence as Seq
import Gift_Pair
import Player
import Test.Hspec

jsonString :: JsonString
jsonString = "{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]}"

player :: Player
player = Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {giver = "JohLen", givee = "GeoHar"}]}

spec :: Spec
spec = do
  describe "playerUpdateGiftHistory" $ do
    it "should return an updated giftHistory" $
      playerUpdateGiftHistory (Seq.fromList [GiftPair {givee = "nope", giver = "yup"}]) player
        `shouldBe` Player "Paul McCartney" (Seq.fromList [GiftPair "nope" "yup"])
  describe "playerJsonStringToPlayer" $ do
    it "should convert from JSON" $ playerJsonStringToPlayer jsonString `shouldBe` Just player
  describe "playerPlayerToJsonString" $ do
    it "should convert to JSON" $ playerPlayerToJsonString player `shouldBe` jsonString

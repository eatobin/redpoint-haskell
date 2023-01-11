module PlayerSpec (spec) where

import qualified Data.Vector as Vec
import Gift_Pair
import Player
import Test.Hspec

jsonString :: JsonString
jsonString = "{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]}"

player :: Player
player = Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {giver = "JohLen", givee = "GeoHar"}]}

spec :: Spec
spec = do
  describe "playerUpdateGiftHistory" $ do
    it "should return an updated giftHistory" $
      playerUpdateGiftHistory (Vec.fromList [GiftPair {givee = "nope", giver = "yup"}]) player
        `shouldBe` Player "Paul McCartney" (Vec.fromList [GiftPair "nope" "yup"])
  describe "playerJsonStringToPlayer" $ do
    it "should convert from JSON" $ playerJsonStringToPlayer jsonString `shouldBe` Just player

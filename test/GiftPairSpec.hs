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
    it " should update a givee" $ giftPairUpdateGivee "NewBee" giftPair `shouldBe` GiftPair "NewBee" "JohLen"

--  describe "setName" $ do
--    it "sets a new Borower name" $ setName "Borrower1" (Borrower "Jack" 1) `shouldBe` br1
--  describe "getMaxBooks" $ do
--    it "returns the Borrower's maxBooks" $ getMaxBooks br1 `shouldBe` 1
--  describe "setMaxBooks" $ do
--    it "sets a new Borower maxBooks" $
--      setMaxBooks 11 br1
--        `shouldBe` Borrower
--          { name = "Borrower1",
--            maxBooks = 11
--          }
--  describe "borrowerToString" $ do
--    it "returns a Borrower as a sting" $
--      borrowerToString br1
--        `shouldBe` "Borrower1 (1 books)"

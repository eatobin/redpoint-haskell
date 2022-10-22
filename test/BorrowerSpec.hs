module BorrowerSpec (spec) where

import Borrower
import Test.Hspec

br1 :: Borrower
br1 = Borrower {name = "Borrower1", maxBooks = 1}

spec :: Spec
spec = do
  describe "getName" $ do
    it "returns the Borrower's name" $ getName br1 `shouldBe` "Borrower1"
  describe "setName" $ do
    it "sets a new Borower name" $ setName "Borrower1" (Borrower "Jack" 1) `shouldBe` br1
  describe "getMaxBooks" $ do
    it "returns the Borrower's maxBooks" $ getMaxBooks br1 `shouldBe` 1
  describe "setMaxBooks" $ do
    it "sets a new Borower maxBooks" $
      setMaxBooks 11 br1
        `shouldBe` Borrower
          { name = "Borrower1",
            maxBooks = 11
          }
  describe "borrowerToString" $ do
    it "returns a Borrower as a sting" $
      borrowerToString br1
        `shouldBe` "Borrower1 (1 books)"

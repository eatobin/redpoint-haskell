module HelpersSpec (spec) where

import Borrower
import Helpers
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

  describe "helpersReadFileIntoJsonString" $ do
    it "given a filepath, returns Either a JsonString or an ErrorString" $
      helpersReadFileIntoJsonString "resources-test/beatles.json"
        `shouldReturn` Right "{\n  \"rosterName\": \"The Beatles\",\n  \"rosterYear\": 2014,\n  \"players\": {\n    \"PauMcc\": {\n      \"playerName\": \"Paul McCartney\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"GeoHar\",\n          \"giver\": \"JohLen\"\n        }\n      ]\n    },\n    \"GeoHar\": {\n      \"playerName\": \"George Harrison\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"RinSta\",\n          \"giver\": \"PauMcc\"\n        }\n      ]\n    },\n    \"JohLen\": {\n      \"playerName\": \"John Lennon\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"PauMcc\",\n          \"giver\": \"RinSta\"\n        }\n      ]\n    },\n    \"RinSta\": {\n      \"playerName\": \"Ringo Starr\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"JohLen\",\n          \"giver\": \"GeoHar\"\n        }\n      ]\n    }\n  }\n}\n"

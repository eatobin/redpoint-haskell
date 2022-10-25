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
    it "given a filepath, returns Either a JsonString or an ErrorString" $ helpersReadFileIntoJsonString "resources/blackhawks.json" `shouldReturn` Right "{\n  \"rosterName\": \"Blackhawks\",\n  \"rosterYear\": 2010,\n  \"players\": {\n    \"TroBro\": {\n      \"playerName\": \"Troy Brouwer\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"DavBol\",\n          \"giver\": \"JoeQue\"\n        }\n      ]\n    },\n    \"JoeQue\": {\n      \"playerName\": \"Joel Quenneville\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"TroBro\",\n          \"giver\": \"AndLad\"\n        }\n      ]\n    },\n    \"AdaBur\": {\n      \"playerName\": \"Adam Burish\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"DunKei\",\n          \"giver\": \"JonToe\"\n        }\n      ]\n    },\n    \"AndLad\": {\n      \"playerName\": \"Andrew Ladd\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"JoeQue\",\n          \"giver\": \"KriVer\"\n        }\n      ]\n    },\n    \"AntNie\": {\n      \"playerName\": \"Antti Niemi\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"JonToe\",\n          \"giver\": \"MarHos\"\n        }\n      ]\n    },\n    \"BreSea\": {\n      \"playerName\": \"Brent Seabrook\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"KriVer\",\n          \"giver\": \"NikHja\"\n        }\n      ]\n    },\n    \"BryBic\": {\n      \"playerName\": \"Bryan Bickell\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"MarHos\",\n          \"giver\": \"PatKan\"\n        }\n      ]\n    },\n    \"CriHue\": {\n      \"playerName\": \"Cristobal Huet\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"PatKan\",\n          \"giver\": \"TomKop\"\n        }\n      ]\n    },\n    \"DavBol\": {\n      \"playerName\": \"Dave Bolland\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"PatSha\",\n          \"giver\": \"TroBro\"\n        }\n      ]\n    },\n    \"DunKei\": {\n      \"playerName\": \"Duncan Keith\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"TomKop\",\n          \"giver\": \"AdaBur\"\n        }\n      ]\n    },\n    \"JonToe\": {\n      \"playerName\": \"Jonathan Toews\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"AdaBur\",\n          \"giver\": \"AntNie\"\n        }\n      ]\n    },\n    \"KriVer\": {\n      \"playerName\": \"Kris Versteeg\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"AndLad\",\n          \"giver\": \"BreSea\"\n        }\n      ]\n    },\n    \"MarHos\": {\n      \"playerName\": \"Marian Hossa\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"AntNie\",\n          \"giver\": \"BryBic\"\n        }\n      ]\n    },\n    \"NikHja\": {\n      \"playerName\": \"Niklas Hjalmarsson\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"BreSea\",\n          \"giver\": \"BriCam\"\n        }\n      ]\n    },\n    \"PatKan\": {\n      \"playerName\": \"Patrick Kane\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"BryBic\",\n          \"giver\": \"CriHue\"\n        }\n      ]\n    },\n    \"PatSha\": {\n      \"playerName\": \"Patrick Sharp\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"BriCam\",\n          \"giver\": \"DavBol\"\n        }\n      ]\n    },\n    \"TomKop\": {\n      \"playerName\": \"Tomas Kopecky\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"CriHue\",\n          \"giver\": \"DunKei\"\n        }\n      ]\n    },\n    \"BriCam\": {\n      \"playerName\": \"Brian Campbell\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"NikHja\",\n          \"giver\": \"PatSha\"\n        }\n      ]\n    }\n  }\n}\n"
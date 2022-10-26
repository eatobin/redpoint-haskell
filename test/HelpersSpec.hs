module HelpersSpec (spec) where

import Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "helpersReadFileIntoJsonString - PASS" $ do
    it "given a valid filepath, returns a Right JsonString" $
      helpersReadFileIntoJsonString "resources-test/bad-json.json"
        `shouldReturn` Right "{\n  \"rosterName\": \"The Beatles\",\n  \"rosterYear\": 2014,\n  \"players\": {\n    \"PauMcc\": {\n      \"playerName\": \"Paul McCartney\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"GeoHar\",\n          \"giver\": \"JohLen\"\n        }\n      ]\n    },\n    \"GeoHar\": {\n      \"playerName\": \"George Harrison\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"RinSta\",\n          \"giver\": \"PauMcc\"\n        }\n      ]\n    },\n    \"JohLen\": {\n      \"playerName\": \"John Lennon\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"PauMcc\",\n          \"giver\": \"RinSta\"\n        }\n      ]\n    },\n    \"RinSta\": {\n      \"playerName\": \"Ringo Starr\",\n      \"giftHistory\": [\n        {\n          \"givee\": \"JohLen\",\n          \"giver\": \"GeoHar\"\n        }\n      ]\n    }\n  }\n}\n"

  describe "helpersReadFileIntoJsonString - FAIL" $ do
    it "given an invalid filepath, returns a Left ErrorString" $
      helpersReadFileIntoJsonString "resources-test/no-file.json"
        `shouldReturn` Left "file read error."

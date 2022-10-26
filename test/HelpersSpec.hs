module HelpersSpec (spec) where

import Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "helpersReadFileIntoJsonString - PASS" $ do
    it "given a valid filepath, returns a Right JsonString" $
      helpersReadFileIntoJsonString "resources-test/bad-json.json"
        `shouldReturn` Right "[ \"test\" :: 123 ]\n"

  describe "helpersReadFileIntoJsonString - FAIL" $ do
    it "given an invalid filepath, returns a Left ErrorString" $
      helpersReadFileIntoJsonString "resources-test/no-file.json"
        `shouldReturn` Left "file read error."

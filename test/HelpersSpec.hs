module HelpersSpec (spec) where

import Helpers
import Test.Hspec
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
--  tvPlayers <- STM.atomically (STM.newTVar Map.empty)
  describe "helpersReadFileIntoJsonString - PASS" $ do
    it "given a valid filepath, returns a Right JsonString" $
      helpersReadFileIntoJsonString "resources-test/bad-json.json"
        `shouldReturn` Right "[ \"test\" :: 123 ]\n"

  describe "helpersReadFileIntoJsonString - FAIL" $ do
    it "given an invalid filepath, returns a Left ErrorString" $
      helpersReadFileIntoJsonString "resources-test/no-file.json"
        `shouldReturn` Left "file read error."

  describe "helpersRosterOrQuit - PASS-1" $ do
    it "given a valid filepath and TVarPlayers, returns an IO (RosterName, RosterYear)" $
        helpersRosterOrQuit "resources-test/no-file.json" Map.empty
          `shouldReturn` ("yes", 42)
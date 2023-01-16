module StateSpec (spec) where

import qualified Data.Set as Set
import Hat
import State
import Test.Hspec

testHat :: Hat
testHat = Set.fromList ["RinSta"]

spec :: Spec
spec = do
  describe "stateDrawPuck" $ do
    it "should draw a puck" $ stateDrawPuck testHat `shouldReturn` Just "RinSta"
    it "should NOT draw a puck" $ stateDrawPuck Set.empty `shouldReturn` Nothing

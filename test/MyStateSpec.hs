module MyStateSpec (spec) where

import qualified Data.Set as Set
import Hat
import MyState
import Test.Hspec

testHat :: Hat
testHat = Set.fromList ["RinSta"]

spec :: Spec
spec = do
  describe "myStateDrawPuck" $ do
    it "should draw a puck from a hat" $ myStateDrawPuck testHat `shouldReturn` Just "RinSta"
    it "should NOT draw a puck from an empty hat" $ myStateDrawPuck Set.empty `shouldReturn` Nothing

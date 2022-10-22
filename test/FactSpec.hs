module FactSpec (spec) where

import Fact -- the code under test
import Test.Hspec -- for unit testing

{- Testing fact -}
spec :: Spec
spec =
  describe "fact" $ do
    context "fact 0" $
      it "should be 1" $
        fact 0 `shouldBe` 1

module StripSpec (spec) where

import Test.Hspec
import Strip

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
    it "also brushes your teeth" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"

  describe "unStrip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
    it "also brushes your teeth" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"

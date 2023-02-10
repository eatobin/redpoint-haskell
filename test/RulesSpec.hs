module RulesSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import Gift_Pair
import Player
import Players
import Rules
import Test.Hspec

beatlesPlusPM :: Players
beatlesPlusPM =
  Map.fromList
    [ ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {giver = "PauMcc", givee = "EriTob"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {giver = "GeoHar", givee = "SusSmi"}]}),
      ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {giver = "JohLen", givee = "DonDuc"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {giver = "RinSta", givee = "MicMou"}]}),
      ("EriTob", Player {playerName = "Eric Tobin", giftHistory = Vec.fromList [GiftPair {giver = "MicMou", givee = "RinSta"}]}),
      ("SusSmi", Player {playerName = "Susan Smith", giftHistory = Vec.fromList [GiftPair {giver = "DonDuc", givee = "JohLen"}]}),
      ("DonDuc", Player {playerName = "Donald Duck", giftHistory = Vec.fromList [GiftPair {giver = "SusSmi", givee = "GeoHar"}]}),
      ("MicMou", Player {playerName = "Mickey Mouse", giftHistory = Vec.fromList [GiftPair {giver = "EriTob", givee = "PauMcc"}]})
    ]

beatlesPlus6 :: Players
beatlesPlus6 =
  let extended = playersAddYear . playersAddYear . playersAddYear . playersAddYear . playersAddYear $ playersAddYear beatlesPlusPM
   in playersUpdateMyGivee "RinSta" "MicMou" 6 . playersUpdateMyGivee "RinSta" "DonDuc" 5 . playersUpdateMyGivee "RinSta" "SusSmi" 4 . playersUpdateMyGivee "RinSta" "EriTob" 3 . playersUpdateMyGivee "RinSta" "PauMcc" 2 $ playersUpdateMyGivee "RinSta" "GeoHar" 1 extended

spec :: Spec
spec = do
  describe "rulesGiveeNotSelf" $ do
    it "A Player should not give to itself - Pass" $ rulesGiveeNotSelf "RinSta" "GeoHar" `shouldBe` True
    it "A Player should not give to itself  - Fail" $ rulesGiveeNotSelf "RinSta" "RinSta" `shouldBe` False

  describe "rulesGiveeNotReciprocal" $ do
    it "A Player should not give to its reciprocal - Pass" $ rulesGiveeNotReciprocal "RinSta" "GeoHar" beatlesPlusPM 0 `shouldBe` True
    it "A Player should not give to its reciprocal - Fail" $ rulesGiveeNotReciprocal "RinSta" "EriTob" beatlesPlusPM 0 `shouldBe` False

  describe "rulesGiveeNotRepeat" $ do
    it "A Player should not repeat a Givee for four years - Pass1" $ rulesGiveeNotRepeat "RinSta" "DonDuc" 2 beatlesPlus6 `shouldBe` True
    it "A Player should not repeat a Givee for four years - Pass2" $ rulesGiveeNotRepeat "RinSta" "PauMcc" 2 beatlesPlus6 `shouldBe` True
    it "A Player should not repeat a Givee for four years - Fail3" $ rulesGiveeNotRepeat "RinSta" "EriTob" 2 beatlesPlus6 `shouldBe` False
    it "A Player should not repeat a Givee for four years - Fail4" $ rulesGiveeNotRepeat "RinSta" "GeoHar" 2 beatlesPlus6 `shouldBe` False
--    it "A Player should not repeat a Givee for four years - Pass5" $ rulesGiveeNotRepeat "RinSta" "GeoHar" 5 beatlesPlus6 `shouldBe` True
--    it "A Player should not repeat a Givee for four years - Fail6" $ rulesGiveeNotRepeat "RinSta" "PauMcc" 5 beatlesPlus6 `shouldBe` False
--    it "A Player should not repeat a Givee for four years - Fail7" $ rulesGiveeNotRepeat "RinSta" "EriTob" 5 beatlesPlus6 `shouldBe` False
--    it "A Player should not repeat a Givee for four years - Fail8" $ rulesGiveeNotRepeat "RinSta" "KarLav" 5 beatlesPlus6 `shouldBe` False

module RulesSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Gift_Pair
import Hat
import Player
import Players
import Rules
import Test.Hspec

reciprocalPlayers :: Players
reciprocalPlayers =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "JohLen"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "GeoHar"}]})
    ]

playersP0 :: Players
playersP0 =
  Map.fromList
    [ ("EriTob", Player {playerName = "Eric Tobin", giftHistory = Seq.fromList [GiftPair {givee = "KarLav", giver = "PauMcc"}]}),
      ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "JohLen"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "RinSta"}]}),
      ("KarLav", Player {playerName = "Karen Lavengood", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "EriTob"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "EriTob", giver = "GeoHar"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "KarLav"}]})
    ]

players1 =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

playersP4 :: Players
playersP4 =
  let extended = playersAddYear . playersAddYear . playersAddYear $ playersAddYear playersP0
   in playersUpdateMyGivee "RinSta" "KarLav" 4 . playersUpdateMyGivee "RinSta" "EriTob" 3 . playersUpdateMyGivee "RinSta" "PauMcc" 2 $ playersUpdateMyGivee "RinSta" "GeoHar" 1 extended

spec :: Spec
spec = do
  describe "rulesGiveeNotSelf" $ do
    it "A Player should not give to itself" $ rulesGiveeNotSelf "JohLen" "GeoHar" `shouldBe` True
  describe "rulesGiveeNotReciprocal" $ do
    it "A Player should not give to its reciprocal - Pass" $ rulesGiveeNotReciprocal "JohLen" "GeoHar" players1 0 `shouldBe` True
    it "A Player should not give to its reciprocal - Fail" $ rulesGiveeNotReciprocal "JohLen" "GeoHar" reciprocalPlayers 0 `shouldBe` False

--  describe "hatDiscardGivee" $ do
--    it "should discard a puck" $ hatDiscardGivee "JohLen" hat2 `shouldBe` hat1
--  describe "hatReturnDiscards" $ do
--    it "should return discarded givees" $ hatReturnDiscards discards hat2 `shouldBe` hat1

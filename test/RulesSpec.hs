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

--reciprocalPlayers :: Players
--reciprocalPlayers =
--  Map.fromList
--    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "JohLen"}]}),
--      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "GeoHar"}]})
--    ]
--
--playersP0 :: Players
--playersP0 =
--  Map.fromList
--    [ ("EriTob", Player {playerName = "Eric Tobin", giftHistory = Vec.fromList [GiftPair {givee = "KarLav", giver = "PauMcc"}]}),
--      ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "JohLen"}]}),
--      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "RinSta"}]}),
--      ("KarLav", Player {playerName = "Karen Lavengood", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "EriTob"}]}),
--      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "EriTob", giver = "GeoHar"}]}),
--      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "KarLav"}]})
--    ]
--
--players1 :: Players
--players1 =
--  Map.fromList
--    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
--      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
--      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
--      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
--    ]
--
--playersP4 :: Players
--playersP4 =
--  let extended = playersAddYear . playersAddYear . playersAddYear $ playersAddYear playersP0
--   in playersUpdateMyGivee "RinSta" "KarLav" 4 . playersUpdateMyGivee "RinSta" "EriTob" 3 . playersUpdateMyGivee "RinSta" "PauMcc" 2 $ playersUpdateMyGivee "RinSta" "GeoHar" 1 extended
--
spec :: Spec
spec = do
  describe "rulesGiveeNotSelf" $ do
    it "A Player should not give to itself" $ rulesGiveeNotSelf "RinSta" "GeoHar" `shouldBe` True
    it "A Player should not give to itself - 2" $ rulesGiveeNotSelf "RinSta" "RinSta" `shouldBe` False

--  describe "rulesGiveeNotReciprocal" $ do
--    it "A Player should not give to its reciprocal - Pass" $ rulesGiveeNotReciprocal "JohLen" "GeoHar" players1 0 `shouldBe` True
--    it "A Player should not give to its reciprocal - Fail" $ rulesGiveeNotReciprocal "JohLen" "GeoHar" reciprocalPlayers 0 `shouldBe` False
--  describe "rulesGiveeNotRepeat" $ do
--    it "A Player should not repeat a Givee for three years - Fail1" $ rulesGiveeNotRepeat "RinSta" "JohLen" 2 playersP4 `shouldBe` False
--    it "A Player should not repeat a Givee for three years - Fail2" $ rulesGiveeNotRepeat "RinSta" "GeoHar" 2 playersP4 `shouldBe` False
--    it "A Player should not repeat a Givee for three years - Pass3" $ rulesGiveeNotRepeat "RinSta" "KarLav" 2 playersP4 `shouldBe` True
--    it "A Player should not repeat a Givee for three years - Pass4" $ rulesGiveeNotRepeat "RinSta" "JohLen" 5 playersP4 `shouldBe` True
--    it "A Player should not repeat a Givee for three years - Pass5" $ rulesGiveeNotRepeat "RinSta" "GeoHar" 5 playersP4 `shouldBe` True
--    it "A Player should not repeat a Givee for three years - Fail6" $ rulesGiveeNotRepeat "RinSta" "PauMcc" 5 playersP4 `shouldBe` False
--    it "A Player should not repeat a Givee for three years - Fail7" $ rulesGiveeNotRepeat "RinSta" "EriTob" 5 playersP4 `shouldBe` False
--    it "A Player should not repeat a Givee for three years - Fail8" $ rulesGiveeNotRepeat "RinSta" "KarLav" 5 playersP4 `shouldBe` False

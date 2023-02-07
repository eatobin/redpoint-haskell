module MyStateSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Gift_Pair
import Hat
import MyState
import Player
import Players
import Test.Hspec

testHat :: Hat
testHat = Set.fromList ["RinSta"]

players0 :: Players
players0 =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

players1 :: Players
players1 =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}, GiftPair {givee = "GeoHar", giver = "GeoHar"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}, GiftPair {givee = "JohLen", giver = "JohLen"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "PauMcc", giver = "PauMcc"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}, GiftPair {givee = "RinSta", giver = "RinSta"}]})
    ]

beatlesState0 :: MyState
beatlesState0 =
  MyState
    { rosterName = "The Beatles",
      rosterYear = 2014,
      players = players0,
      giftYear = 0,
      giveeHat = Set.empty,
      giverHat = Set.empty,
      maybeGivee = Nothing,
      maybeGiver = Nothing,
      discards = Set.empty,
      quit = "n"
    }

spec :: Spec
spec = do
  drawPuck
  startNewYear
  selectNewGiver

drawPuck :: Spec
drawPuck = do
  describe "myStateDrawPuck" $ do
    it "should draw a puck from a hat" $ myStateDrawPuck testHat `shouldReturn` Just "RinSta"
    it "should NOT draw a puck from an empty hat" $ myStateDrawPuck Set.empty `shouldReturn` Nothing

startNewYear :: Spec
startNewYear = beforeAll (myStateStartNewYear (return beatlesState0)) $ do
  describe "myStateStartNewYear" $ do
    it "should update players" $ \beatlesState1 -> do
      players beatlesState1 `shouldBe` players1
    it "should update giftYear" $ \beatlesState1 -> do
      giftYear beatlesState1 `shouldBe` 1
    it "should update giveeHat" $ \beatlesState1 -> do
      giveeHat beatlesState1 `shouldBe` Set.fromList ["GeoHar", "JohLen", "PauMcc", "RinSta"]
    it "should update giverHat" $ \beatlesState1 -> do
      giverHat beatlesState1 `shouldBe` Set.fromList ["GeoHar", "JohLen", "PauMcc", "RinSta"]
    it "should update maybeGivee" $ \beatlesState1 -> do
      maybeGivee beatlesState1 `shouldNotBe` (Nothing :: Maybe Givee)
    it "should update maybeGiver" $ \beatlesState1 -> do
      maybeGiver beatlesState1 `shouldNotBe` (Nothing :: Maybe Giver)

selectNewGiver :: Spec
selectNewGiver = beforeAll (myStateStartNewYear (return beatlesState0)) $ do
  describe "myStateSelectNewGiver" $ do
    it "should discard correctly" $ \beatlesState1 -> do
      let newDiscards = hatDiscardGivee "GeoHar" (discards beatlesState1)
      length newDiscards `shouldBe` 1
    it "should draw a new giver correctly" $ \beatlesState1 -> do
          let secondStateIO = myStateSelectNewGiver (return beatlesState1)
          do
            secondState <- secondStateIO
            length (giverHat secondState) `shouldBe` 3

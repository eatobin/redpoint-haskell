module StateSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Gift_Pair
import Hat
import Player
import Players
import State
import Test.Hspec

testHat :: Hat
testHat = Set.fromList ["RinSta"]

stateSpecPlayers :: Players
stateSpecPlayers =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

-- TODO broken
beatlesState :: State
beatlesState =
  State
    { rosterName = "The Beatles",
      rosterYear = 2014,
      players = stateSpecPlayers,
      giftYear = 0,
      giveeHat = Set.empty,
      giverHat = Set.empty,
      maybeGivee = IO Nothing,
      maybeGiver = IO Nothing,
      discards = Set.empty,
      quit = "n"
    }

spec :: Spec
spec = do
  describe "stateDrawPuck" $ do
    it "should draw a puck" $ stateDrawPuck testHat `shouldReturn` Just "RinSta"
    it "should NOT draw a puck" $ stateDrawPuck Set.empty `shouldReturn` Nothing

--
--  describe "stateStartNewYear" $ do
--    it "start a new year" $ stateDrawPuck testHat `shouldReturn` Just "RinSta"

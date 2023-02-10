module MyStateSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM
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
  giveeIsFailure
  giveeIsSuccess
  selectNewGiver

drawPuck :: Spec
drawPuck = do
  describe "myStateDrawPuck" $ do
    it "should draw a puck from a hat" $ myStateDrawPuck testHat `shouldReturn` Just "RinSta"
    it "should NOT draw a puck from an empty hat" $ myStateDrawPuck Set.empty `shouldReturn` Nothing

startNewYear :: Spec
startNewYear = beforeAll (myStateStartNewYear (return beatlesState0)) $ do
  describe "myStateStartNewYear" $ do
    it "should update players" $ \newState -> do
      players newState `shouldBe` players1
    it "should update giftYear" $ \newState -> do
      giftYear newState `shouldBe` 1
    it "should update giveeHat" $ \newState -> do
      giveeHat newState `shouldBe` Set.fromList ["GeoHar", "JohLen", "PauMcc", "RinSta"]
    it "should update giverHat" $ \newState -> do
      giverHat newState `shouldBe` Set.fromList ["GeoHar", "JohLen", "PauMcc", "RinSta"]
    it "should update maybeGivee" $ \newState -> do
      maybeGivee newState `shouldNotBe` (Nothing :: Maybe Givee)
    it "should update maybeGiver" $ \newState -> do
      maybeGiver newState `shouldNotBe` (Nothing :: Maybe Giver)

giveeIsFailure :: Spec
giveeIsFailure = beforeAll (myStateStartNewYear (return beatlesState0)) $ do
  describe "myStateGiveeIsFailure" $ do
    it "should have a failing givee" $ \beatlesState1 -> do
      let badGivee = DM.fromJust (maybeGivee beatlesState1)
      let beatlesState2IO = myStateGiveeIsFailure (return beatlesState1)
      do
        beatlesState2 <- beatlesState2IO
        Set.notMember badGivee (giveeHat beatlesState2) `shouldBe` True
        DM.fromJust (maybeGivee beatlesState2) `shouldNotBe` badGivee
        Set.member badGivee (discards beatlesState2) `shouldBe` True

giveeIsSuccess :: Spec
giveeIsSuccess = beforeAll (myStateStartNewYear (return beatlesState0)) $ do
  describe "myStateGiveeIsSuccess" $ do
    it "have a successful givee" $ \beatlesState1 -> do
      let goodGivee = DM.fromJust (maybeGivee beatlesState1)
      let goodGiver = DM.fromJust (maybeGiver beatlesState1)
      let beatlesState2IO = myStateGiveeIsSuccess (return beatlesState1)
      do
        beatlesState2 <- beatlesState2IO
        playersGetMyGivee goodGiver (players beatlesState2) (giftYear beatlesState2) `shouldBe` goodGivee
        playersGetMyGiver goodGivee (players beatlesState2) (giftYear beatlesState2) `shouldBe` goodGiver
        Set.notMember goodGivee (giveeHat beatlesState2) `shouldBe` True
        DM.isNothing (maybeGivee beatlesState2) `shouldBe` True

selectNewGiver :: Spec
selectNewGiver = beforeAll (myStateStartNewYear (return beatlesState0)) $ do
  describe "myStateSelectNewGiver" $ do
    it "select a new giver" $ \beatlesState1 -> do
      let badGivee = DM.fromJust (maybeGivee beatlesState1)
      let goodGivee = DM.fromJust (maybeGivee beatlesState1)
      let goodGiver = DM.fromJust (maybeGiver beatlesState1)
      let beatlesState2IO = myStateGiveeIsSuccess (return beatlesState1)
      do
        beatlesState2 <- beatlesState2IO
        playersGetMyGivee goodGiver (players beatlesState2) (giftYear beatlesState2) `shouldBe` goodGivee
        playersGetMyGiver goodGivee (players beatlesState2) (giftYear beatlesState2) `shouldBe` goodGiver
        Set.notMember goodGivee (giveeHat beatlesState2) `shouldBe` True
        DM.isNothing (maybeGivee beatlesState2) `shouldBe` True
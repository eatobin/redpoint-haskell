{-# LANGUAGE QuasiQuotes #-}

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
import Text.RawString.QQ (r)

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

playersWeird :: Players
playersWeird =
  Map.fromList
    [ ("GeoHar", Player {playerName = "geoWhoops", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "pauYikes", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "PauMcc"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

rinStaPlus :: Player
rinStaPlus =
  Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}, GiftPair {givee = "RinSta", giver = "RinSta"}]}

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

weirdState :: MyState
weirdState =
  MyState
    { rosterName = "The Beatles",
      rosterYear = 2014,
      players = playersWeird,
      giftYear = 0,
      giveeHat = Set.empty,
      giverHat = Set.empty,
      maybeGivee = Nothing,
      maybeGiver = Nothing,
      discards = Set.empty,
      quit = "n"
    }

beatlesJson :: JsonString
beatlesJson = [r|{"rosterName":"The Beatles","rosterYear":2014,"players":{"RinSta":{"playerName":"Ringo Starr","giftHistory":[{"givee":"JohLen","giver":"GeoHar"}]},"JohLen":{"playerName":"John Lennon","giftHistory":[{"givee":"PauMcc","giver":"RinSta"}]},"GeoHar":{"playerName":"George Harrison","giftHistory":[{"givee":"RinSta","giver":"PauMcc"}]},"PauMcc":{"playerName":"Paul McCartney","giftHistory":[{"givee":"GeoHar","giver":"JohLen"}]}},"giftYear":0,"giveeHat":[],"giverHat":[],"maybeGivee":null,"maybeGiver":null,"discards":[],"quit":"n"}|]

hawksJson :: JsonString
hawksJson = [r|{"rosterName":"Blackhawks","rosterYear":2010,"players":{"TroBro":{"playerName":"Troy Brouwer","giftHistory":[{"givee":"DavBol","giver":"JoeQue"}]},"PatKan":{"playerName":"Patrick Kane","giftHistory":[{"givee":"BryBic","giver":"CriHue"}]},"JoeQue":{"playerName":"Joel Quenneville","giftHistory":[{"givee":"TroBro","giver":"AndLad"}]},"NikHja":{"playerName":"Niklas Hjalmarsson","giftHistory":[{"givee":"BreSea","giver":"BriCam"}]},"TomKop":{"playerName":"Tomas Kopecky","giftHistory":[{"givee":"CriHue","giver":"DunKei"}]},"BryBic":{"playerName":"Bryan Bickell","giftHistory":[{"givee":"MarHos","giver":"PatKan"}]},"AntNie":{"playerName":"Antti Niemi","giftHistory":[{"givee":"JonToe","giver":"MarHos"}]},"PatSha":{"playerName":"Patrick Sharp","giftHistory":[{"givee":"BriCam","giver":"DavBol"}]},"DunKei":{"playerName":"Duncan Keith","giftHistory":[{"givee":"TomKop","giver":"AdaBur"}]},"BriCam":{"playerName":"Brian Campbell","giftHistory":[{"givee":"NikHja","giver":"PatSha"}]},"BreSea":{"playerName":"Brent Seabrook","giftHistory":[{"givee":"KriVer","giver":"NikHja"}]},"KriVer":{"playerName":"Kris Versteeg","giftHistory":[{"givee":"AndLad","giver":"BreSea"}]},"MarHos":{"playerName":"Marian Hossa","giftHistory":[{"givee":"AntNie","giver":"BryBic"}]},"AndLad":{"playerName":"Andrew Ladd","giftHistory":[{"givee":"JoeQue","giver":"KriVer"}]},"DavBol":{"playerName":"Dave Bolland","giftHistory":[{"givee":"PatSha","giver":"TroBro"}]},"CriHue":{"playerName":"Cristobal Huet","giftHistory":[{"givee":"PatKan","giver":"TomKop"}]},"JonToe":{"playerName":"Jonathan Toews","giftHistory":[{"givee":"AdaBur","giver":"AntNie"}]},"AdaBur":{"playerName":"Adam Burish","giftHistory":[{"givee":"DunKei","giver":"JonToe"}]}},"giftYear":0,"giveeHat":[],"giverHat":[],"maybeGivee":null,"maybeGiver":null,"discards":[],"quit":"n"}|]

spec :: Spec
spec = do
  drawPuck
  startNewYear
  giveeIsFailure

--  giveeIsSuccess
--  selectNewGiver
--  errors
--  printResults
--  convertFromJSON

drawPuck :: Spec
drawPuck = do
  describe "myStateDrawPuck" $ do
    it "should draw a puck from a hat" $ myStateDrawPuck testHat `shouldReturn` Just "RinSta"
    it "should NOT draw a puck from an empty hat" $ myStateDrawPuck Set.empty `shouldReturn` Nothing

startNewYear :: Spec
startNewYear = beforeAll (myStateStartNewYear beatlesState0) $ do
  describe "myStateStartNewYear" $ do
    it "should update players" $ \beatlesState1 -> do
      players beatlesState1 Map.! "RinSta" `shouldBe` rinStaPlus
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

giveeIsFailure :: Spec
giveeIsFailure = beforeAll (myStateStartNewYear beatlesState0) $ do
  describe "myStateGiveeIsFailure" $ do
    it "should have a failing givee" $ \beatlesState1 -> do
      let badGivee = DM.fromJust (maybeGivee beatlesState1)
      let beatlesState2IO = myStateGiveeIsFailure beatlesState1
      do
        beatlesState2 <- beatlesState2IO
        Set.notMember badGivee (giveeHat beatlesState2) `shouldBe` True
        DM.fromJust (maybeGivee beatlesState2) `shouldNotBe` badGivee
        Set.member badGivee (discards beatlesState2) `shouldBe` True

--giveeIsSuccess :: Spec
--giveeIsSuccess = beforeAll (myStateStartNewYear (return beatlesState0)) $ do
--  describe "myStateGiveeIsSuccess" $ do
--    it "should have a successful givee" $ \beatlesState1 -> do
--      let goodGivee = DM.fromJust (maybeGivee beatlesState1)
--      let goodGiver = DM.fromJust (maybeGiver beatlesState1)
--      let beatlesState2IO = myStateGiveeIsSuccess (return beatlesState1)
--      do
--        beatlesState2 <- beatlesState2IO
--        playersGetMyGivee goodGiver (players beatlesState2) (giftYear beatlesState2) `shouldBe` goodGivee
--        playersGetMyGiver goodGivee (players beatlesState2) (giftYear beatlesState2) `shouldBe` goodGiver
--        Set.notMember goodGivee (giveeHat beatlesState2) `shouldBe` True
--        DM.isNothing (maybeGivee beatlesState2) `shouldBe` True
--
--selectNewGiver :: Spec
--selectNewGiver = beforeAll (myStateStartNewYear (return beatlesState0)) $ do
--  describe "myStateSelectNewGiver" $ do
--    it "should select a new giver" $ \beatlesState1 -> do
--      let badGivee = DM.fromJust (maybeGivee beatlesState1)
--      let beatlesState2IO = myStateGiveeIsFailure (return beatlesState1)
--      do
--        beatlesState2 <- beatlesState2IO
--        let goodGivee = DM.fromJust (maybeGivee beatlesState2)
--        let goodGiver = DM.fromJust (maybeGiver beatlesState2)
--        let beatlesState3IO = myStateGiveeIsSuccess (return beatlesState2)
--        let beatlesState4IO = myStateSelectNewGiver beatlesState3IO
--        do
--          beatlesState4 <- beatlesState4IO
--          Set.member badGivee (giveeHat beatlesState4) `shouldBe` True
--          Set.notMember goodGivee (giveeHat beatlesState4) `shouldBe` True
--          Set.notMember goodGiver (giverHat beatlesState4) `shouldBe` True
--          DM.fromJust (maybeGivee beatlesState4) `shouldNotBe` goodGivee
--          DM.fromJust (maybeGiver beatlesState4) `shouldNotBe` goodGiver
--          null (discards beatlesState4) `shouldBe` True
--
--errors :: Spec
--errors = do
--  describe "myStateErrors" $ do
--    it "should report player errors" $ myStateErrors weirdState `shouldBe` ["GeoHar", "PauMcc"]
--
--printResults :: Spec
--printResults = do
--  describe "myStatePrintResults" $ do
--    it "should print itself and return itself - beatlesState0" $ myStatePrintResults (return beatlesState0) `shouldReturn` beatlesState0
--    it "should print itself and return itself - weirdState" $ myStatePrintResults (return weirdState) `shouldReturn` weirdState
--
--convertFromJSON :: Spec
--convertFromJSON = do
--  describe "myStateJsonStringToMyState" $ do
--    it "convert from JSON-Beatles" $ myStateJsonStringToMyState beatlesJson `shouldBe` Just beatlesState0
--    it "convert from JSON-Hawks" $ myStateJsonStringToMyState hawksJson `shouldBe` Just (MyState {rosterName = "Blackhawks", rosterYear = 2010, players = Map.fromList [("AdaBur", Player {playerName = "Adam Burish", giftHistory = Vec.fromList [GiftPair {givee = "DunKei", giver = "JonToe"}]}), ("AndLad", Player {playerName = "Andrew Ladd", giftHistory = Vec.fromList [GiftPair {givee = "JoeQue", giver = "KriVer"}]}), ("AntNie", Player {playerName = "Antti Niemi", giftHistory = Vec.fromList [GiftPair {givee = "JonToe", giver = "MarHos"}]}), ("BreSea", Player {playerName = "Brent Seabrook", giftHistory = Vec.fromList [GiftPair {givee = "KriVer", giver = "NikHja"}]}), ("BriCam", Player {playerName = "Brian Campbell", giftHistory = Vec.fromList [GiftPair {givee = "NikHja", giver = "PatSha"}]}), ("BryBic", Player {playerName = "Bryan Bickell", giftHistory = Vec.fromList [GiftPair {givee = "MarHos", giver = "PatKan"}]}), ("CriHue", Player {playerName = "Cristobal Huet", giftHistory = Vec.fromList [GiftPair {givee = "PatKan", giver = "TomKop"}]}), ("DavBol", Player {playerName = "Dave Bolland", giftHistory = Vec.fromList [GiftPair {givee = "PatSha", giver = "TroBro"}]}), ("DunKei", Player {playerName = "Duncan Keith", giftHistory = Vec.fromList [GiftPair {givee = "TomKop", giver = "AdaBur"}]}), ("JoeQue", Player {playerName = "Joel Quenneville", giftHistory = Vec.fromList [GiftPair {givee = "TroBro", giver = "AndLad"}]}), ("JonToe", Player {playerName = "Jonathan Toews", giftHistory = Vec.fromList [GiftPair {givee = "AdaBur", giver = "AntNie"}]}), ("KriVer", Player {playerName = "Kris Versteeg", giftHistory = Vec.fromList [GiftPair {givee = "AndLad", giver = "BreSea"}]}), ("MarHos", Player {playerName = "Marian Hossa", giftHistory = Vec.fromList [GiftPair {givee = "AntNie", giver = "BryBic"}]}), ("NikHja", Player {playerName = "Niklas Hjalmarsson", giftHistory = Vec.fromList [GiftPair {givee = "BreSea", giver = "BriCam"}]}), ("PatKan", Player {playerName = "Patrick Kane", giftHistory = Vec.fromList [GiftPair {givee = "BryBic", giver = "CriHue"}]}), ("PatSha", Player {playerName = "Patrick Sharp", giftHistory = Vec.fromList [GiftPair {givee = "BriCam", giver = "DavBol"}]}), ("TomKop", Player {playerName = "Tomas Kopecky", giftHistory = Vec.fromList [GiftPair {givee = "CriHue", giver = "DunKei"}]}), ("TroBro", Player {playerName = "Troy Brouwer", giftHistory = Vec.fromList [GiftPair {givee = "DavBol", giver = "JoeQue"}]})], giftYear = 0, giveeHat = Set.fromList [], giverHat = Set.fromList [], maybeGivee = Nothing, maybeGiver = Nothing, discards = Set.fromList [], quit = "n"})

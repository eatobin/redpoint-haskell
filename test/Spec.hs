{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import GiftHistory
import GiftPair
import Hat
import MyState
import Player
import Players
import Rules
import Test.Hspec
import Text.RawString.QQ (r)

-- giftPair
giftPairJsonString :: JsonString
giftPairJsonString = "{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}"

giftPairStruct :: GiftPairStruct
giftPairStruct = GiftPairStruct {giver = "JohLen", givee = "GeoHar"}

-- giftHistory
giftHistoryJsonString :: JsonString
giftHistoryJsonString = "[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]"

giftHistoryVector :: GiftHistoryVector
giftHistoryVector = Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "JohLen"}]

-- player
playerJsonString :: JsonString
playerJsonString = "{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]}"

playerStruct :: PlayerStruct
playerStruct = PlayerStruct {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPairStruct {giver = "JohLen", givee = "GeoHar"}]}

-- players
playersJsonString :: JsonString
playersJsonString = "{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}"

playerGeoHar :: PlayerStruct
playerGeoHar = PlayerStruct {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPairStruct {givee = "RinSta", giver = "PauMcc"}]}

playersSpecPlayers :: PlayersMap
playersSpecPlayers =
  Map.fromList
    [ ("GeoHar", playerGeoHar),
      ("JohLen", PlayerStruct {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPairStruct {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", PlayerStruct {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", PlayerStruct {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPairStruct {givee = "JohLen", giver = "GeoHar"}]})
    ]

newBeePlayers :: PlayersMap
newBeePlayers =
  Map.fromList
    [ ("GeoHar", PlayerStruct {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPairStruct {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", PlayerStruct {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPairStruct {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", PlayerStruct {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", PlayerStruct {playerName = "New Bee", giftHistory = Vec.fromList [GiftPairStruct {givee = "NewBee", giver = "NewBee"}]})
    ]

playersExt :: PlayersMap
playersExt =
  Map.fromList
    [ ("GeoHar", PlayerStruct {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPairStruct {givee = "RinSta", giver = "PauMcc"}, GiftPairStruct {givee = "GeoHar", giver = "GeoHar"}]}),
      ("JohLen", PlayerStruct {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPairStruct {givee = "PauMcc", giver = "RinSta"}, GiftPairStruct {givee = "JohLen", giver = "JohLen"}]}),
      ("PauMcc", PlayerStruct {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "JohLen"}, GiftPairStruct {givee = "PauMcc", giver = "PauMcc"}]}),
      ("RinSta", PlayerStruct {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPairStruct {givee = "JohLen", giver = "GeoHar"}, GiftPairStruct {givee = "RinSta", giver = "RinSta"}]})
    ]

playersGivee :: PlayersMap
playersGivee =
  Map.fromList
    [ ("GeoHar", PlayerStruct {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPairStruct {givee = "you", giver = "PauMcc"}]}),
      ("JohLen", PlayerStruct {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPairStruct {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", PlayerStruct {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", PlayerStruct {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPairStruct {givee = "JohLen", giver = "GeoHar"}]})
    ]

playersGiver :: PlayersMap
playersGiver =
  Map.fromList
    [ ("GeoHar", PlayerStruct {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPairStruct {givee = "RinSta", giver = "you"}]}),
      ("JohLen", PlayerStruct {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPairStruct {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", PlayerStruct {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", PlayerStruct {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPairStruct {givee = "JohLen", giver = "GeoHar"}]})
    ]

players1 :: PlayersMap
players1 =
  Map.fromList
    [ ("GeoHar", PlayerStruct {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPairStruct {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", PlayerStruct {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPairStruct {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", PlayerStruct {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", PlayerStruct {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPairStruct {givee = "JohLen", giver = "GeoHar"}]})
    ]

-- hat
hatHat1 :: HatSet
hatHat1 = Set.fromList ["RinSta", "GeoHar", "JohLen", "PauMcc", "JohLen"]

hat2 :: HatSet
hat2 = Set.fromList ["RinSta", "GeoHar", "PauMcc"]

testDiscards :: HatSet
testDiscards = Set.fromList ["JohLen"]

-- rules
beatlesPlusPM :: PlayersMap
beatlesPlusPM =
  Map.fromList
    [ ("RinSta", PlayerStruct {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPairStruct {giver = "PauMcc", givee = "EriTob"}]}),
      ("JohLen", PlayerStruct {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPairStruct {giver = "GeoHar", givee = "SusSmi"}]}),
      ("GeoHar", PlayerStruct {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPairStruct {giver = "JohLen", givee = "DonDuc"}]}),
      ("PauMcc", PlayerStruct {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPairStruct {giver = "RinSta", givee = "MicMou"}]}),
      ("EriTob", PlayerStruct {playerName = "Eric Tobin", giftHistory = Vec.fromList [GiftPairStruct {giver = "MicMou", givee = "RinSta"}]}),
      ("SusSmi", PlayerStruct {playerName = "Susan Smith", giftHistory = Vec.fromList [GiftPairStruct {giver = "DonDuc", givee = "JohLen"}]}),
      ("DonDuc", PlayerStruct {playerName = "Donald Duck", giftHistory = Vec.fromList [GiftPairStruct {giver = "SusSmi", givee = "GeoHar"}]}),
      ("MicMou", PlayerStruct {playerName = "Mickey Mouse", giftHistory = Vec.fromList [GiftPairStruct {giver = "EriTob", givee = "PauMcc"}]})
    ]

beatlesPlus6 :: PlayersMap
beatlesPlus6 =
  let extended = playersAddYear . playersAddYear . playersAddYear . playersAddYear . playersAddYear $ playersAddYear beatlesPlusPM
   in playersUpdateMyGivee "RinSta" "MicMou" 6 . playersUpdateMyGivee "RinSta" "DonDuc" 5 . playersUpdateMyGivee "RinSta" "SusSmi" 4 . playersUpdateMyGivee "RinSta" "EriTob" 3 . playersUpdateMyGivee "RinSta" "PauMcc" 2 $ playersUpdateMyGivee "RinSta" "GeoHar" 1 extended

-- myState
testHat :: HatSet
testHat = Set.fromList ["RinSta"]

players0 :: PlayersMap
players0 =
  Map.fromList
    [ ("GeoHar", PlayerStruct {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPairStruct {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", PlayerStruct {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPairStruct {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", PlayerStruct {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", PlayerStruct {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPairStruct {givee = "JohLen", giver = "GeoHar"}]})
    ]

playersWeird :: PlayersMap
playersWeird =
  Map.fromList
    [ ("GeoHar", PlayerStruct {playerName = "geoWhoops", giftHistory = Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "PauMcc"}]}),
      ("JohLen", PlayerStruct {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPairStruct {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", PlayerStruct {playerName = "pauYikes", giftHistory = Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "PauMcc"}]}),
      ("RinSta", PlayerStruct {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPairStruct {givee = "JohLen", giver = "GeoHar"}]})
    ]

rinStaPlus :: PlayerStruct
rinStaPlus =
  PlayerStruct {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPairStruct {givee = "JohLen", giver = "GeoHar"}, GiftPairStruct {givee = "RinSta", giver = "RinSta"}]}

beatlesState0 :: MyStateStruct
beatlesState0 =
  MyStateStruct
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

weirdState :: MyStateStruct
weirdState =
  MyStateStruct
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

main :: IO ()
main = hspec $ do
  -- giftPair
  describe "giftPairUpdateGivee" $ do
    it "should update a givee" $ giftPairUpdateGivee "NewBee" giftPairStruct `shouldBe` GiftPairStruct "NewBee" "JohLen"

  describe "giftPairUpdateGiver" $ do
    it "should update a giver" $ giftPairUpdateGiver "NewBee" giftPairStruct `shouldBe` GiftPairStruct "GeoHar" "NewBee"

  describe "giftPairJsonStringToGiftPairStruct" $ do
    it "should convert from JSON" $ giftPairJsonStringToGiftPairStruct giftPairJsonString `shouldBe` Just giftPairStruct

  -- giftHistory
  describe "\ngiftHistoryAddYear" $ do
    it "should add a new year" $ giftHistoryAddYear "NewBee" giftHistoryVector `shouldBe` Vec.fromList [GiftPairStruct {givee = "GeoHar", giver = "JohLen"}, GiftPairStruct {givee = "NewBee", giver = "NewBee"}]

  describe "giftHistoryUpdateGiftHistory" $ do
    it "should return an updated giftHistoryVector" $ giftHistoryUpdateGiftHistoryVector 0 (GiftPairStruct "me" "you") giftHistoryVector `shouldBe` Vec.fromList [GiftPairStruct {givee = "me", giver = "you"}]

  describe "giftHistoryJsonStringToGiftHistoryVector" $ do
    it "should convert from JSON" $ giftHistoryJsonStringToGiftHistoryVector giftHistoryJsonString `shouldBe` Just giftHistoryVector

  -- player
  describe "\nplayerUpdateGiftHistory" $ do
    it "should return an updated giftHistory" $
      playerUpdateGiftHistory (Vec.fromList [GiftPairStruct {givee = "nope", giver = "yup"}]) playerStruct
        `shouldBe` PlayerStruct "Paul McCartney" (Vec.fromList [GiftPairStruct "nope" "yup"])

  describe "playerJsonStringToPlayerStruct" $ do
    it "should convert from JSON" $ playerJsonStringToPlayerStruct playerJsonString `shouldBe` Just playerStruct

  -- players
  describe "\nplayersUpdatePlayer" $ do
    it "should return an updated player" $ playersUpdatePlayer "RinSta" PlayerStruct {playerName = "New Bee", giftHistory = Vec.fromList [GiftPairStruct {giver = "NewBee", givee = "NewBee"}]} playersSpecPlayers `shouldBe` newBeePlayers

  describe "playersGetPlayerName" $ do
    it "should return a player name" $ playersGetPlayerName "PauMcc" playersSpecPlayers `shouldBe` "Paul McCartney"

  describe "playersAddYear" $ do
    it "should add a new year" $ playersAddYear playersSpecPlayers `shouldBe` playersExt

  describe "playersGetMyGivee and playersGetMyGiver" $ do
    it "should return a givee" $ playersGetMyGivee "JohLen" playersSpecPlayers 0 `shouldBe` "PauMcc"
    it "should return a giver" $ playersGetMyGiver "JohLen" playersSpecPlayers 0 `shouldBe` "RinSta"

  describe "playersUpdateMyGivee and playersUpdateMyGiver" $ do
    it "should update a givee" $ playersUpdateMyGivee "GeoHar" "you" 0 playersSpecPlayers `shouldBe` playersGivee
    it "should update a giver" $ playersUpdateMyGiver "GeoHar" "you" 0 playersSpecPlayers `shouldBe` playersGiver

  describe "playersJsonStringToPlayers" $ do
    it "should convert from JSON" $ playersJsonStringToPlayersMap playersJsonString `shouldBe` Just playersSpecPlayers

  -- hat
  describe "\nhatMakeHat" $ do
    it "should make itself given players" $ hatMakeHat players1 `shouldBe` hatHat1
  describe "hatRemovePuck" $ do
    it "should remove a puck" $ hatRemovePuck "JohLen" hatHat1 `shouldBe` hat2
  describe "hatDiscardGivee" $ do
    it "should discard a puck" $ hatDiscardGivee "JohLen" hat2 `shouldBe` hatHat1
  describe "hatReturnDiscards" $ do
    it "should return discarded givees" $ hatReturnDiscards testDiscards hat2 `shouldBe` hatHat1

  -- rules
  describe "\nrulesGiveeNotSelf" $ do
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
    it "A Player should not repeat a Givee for four years - Fail5" $ rulesGiveeNotRepeat "RinSta" "MicMou" 7 beatlesPlus6 `shouldBe` False
    it "A Player should not repeat a Givee for four years - Pass6" $ rulesGiveeNotRepeat "RinSta" "MicMou" 6 beatlesPlus6 `shouldBe` True
    it "A Player should not repeat a Givee for four years - Fail7" $ rulesGiveeNotRepeat "RinSta" "PauMcc" 6 beatlesPlus6 `shouldBe` False
    it "A Player should not repeat a Givee for four years - Pass8" $ rulesGiveeNotRepeat "RinSta" "GeoHar" 6 beatlesPlus6 `shouldBe` True

  -- myState
  drawPuck
  startNewYear
  giveeIsFailure
  giveeIsSuccess
  selectNewGiver
  errors
  printResults
  convertFromJSON

drawPuck :: Spec
drawPuck = do
  describe "\nmyStateDrawPuck" $ do
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

giveeIsSuccess :: Spec
giveeIsSuccess = beforeAll (myStateStartNewYear beatlesState0) $ do
  describe "myStateGiveeIsSuccess" $ do
    it "should have a successful givee" $ \beatlesState1 -> do
      let goodGivee = DM.fromJust (maybeGivee beatlesState1)
      let goodGiver = DM.fromJust (maybeGiver beatlesState1)
      let beatlesState2IO = myStateGiveeIsSuccess beatlesState1
      do
        beatlesState2 <- beatlesState2IO
        playersGetMyGivee goodGiver (players beatlesState2) (giftYear beatlesState2) `shouldBe` goodGivee
        playersGetMyGiver goodGivee (players beatlesState2) (giftYear beatlesState2) `shouldBe` goodGiver
        Set.notMember goodGivee (giveeHat beatlesState2) `shouldBe` True
        DM.isNothing (maybeGivee beatlesState2) `shouldBe` True

selectNewGiver :: Spec
selectNewGiver = beforeAll (myStateStartNewYear beatlesState0) $ do
  describe "myStateSelectNewGiver" $ do
    it "should select a new giver" $ \beatlesState1 -> do
      let badGivee = DM.fromJust (maybeGivee beatlesState1)
      let beatlesState2IO = myStateGiveeIsFailure beatlesState1
      do
        beatlesState2 <- beatlesState2IO
        let goodGivee = DM.fromJust (maybeGivee beatlesState2)
        let goodGiver = DM.fromJust (maybeGiver beatlesState2)
        let beatlesState3IO = myStateGiveeIsSuccess beatlesState2
        beatlesState3 <- beatlesState3IO
        let beatlesState4IO = myStateSelectNewGiver beatlesState3
        do
          beatlesState4 <- beatlesState4IO
          Set.member badGivee (giveeHat beatlesState4) `shouldBe` True
          Set.notMember goodGivee (giveeHat beatlesState4) `shouldBe` True
          Set.notMember goodGiver (giverHat beatlesState4) `shouldBe` True
          DM.fromJust (maybeGivee beatlesState4) `shouldNotBe` goodGivee
          DM.fromJust (maybeGiver beatlesState4) `shouldNotBe` goodGiver
          null (discards beatlesState4) `shouldBe` True

errors :: Spec
errors = do
  describe "myStateErrors" $ do
    it "should report player errors" $ myStateErrors weirdState `shouldBe` ["GeoHar", "PauMcc"]

printResults :: Spec
printResults = do
  describe "myStatePrintResults" $ do
    it "should print itself and return itself - beatlesState0" $ myStatePrintResults beatlesState0 `shouldReturn` beatlesState0
    it "should print itself and return itself - weirdState" $ myStatePrintResults weirdState `shouldReturn` weirdState

convertFromJSON :: Spec
convertFromJSON = do
  describe "myStateJsonStringToMyStateStruct" $ do
    it "convert from JSON-Beatles" $ myStateJsonStringToMyStateStruct beatlesJson `shouldBe` Just beatlesState0
    it "convert from JSON-Hawks" $ myStateJsonStringToMyStateStruct hawksJson `shouldBe` Just (MyStateStruct {rosterName = "Blackhawks", rosterYear = 2010, players = Map.fromList [("AdaBur", PlayerStruct {playerName = "Adam Burish", giftHistory = Vec.fromList [GiftPairStruct {givee = "DunKei", giver = "JonToe"}]}), ("AndLad", PlayerStruct {playerName = "Andrew Ladd", giftHistory = Vec.fromList [GiftPairStruct {givee = "JoeQue", giver = "KriVer"}]}), ("AntNie", PlayerStruct {playerName = "Antti Niemi", giftHistory = Vec.fromList [GiftPairStruct {givee = "JonToe", giver = "MarHos"}]}), ("BreSea", PlayerStruct {playerName = "Brent Seabrook", giftHistory = Vec.fromList [GiftPairStruct {givee = "KriVer", giver = "NikHja"}]}), ("BriCam", PlayerStruct {playerName = "Brian Campbell", giftHistory = Vec.fromList [GiftPairStruct {givee = "NikHja", giver = "PatSha"}]}), ("BryBic", PlayerStruct {playerName = "Bryan Bickell", giftHistory = Vec.fromList [GiftPairStruct {givee = "MarHos", giver = "PatKan"}]}), ("CriHue", PlayerStruct {playerName = "Cristobal Huet", giftHistory = Vec.fromList [GiftPairStruct {givee = "PatKan", giver = "TomKop"}]}), ("DavBol", PlayerStruct {playerName = "Dave Bolland", giftHistory = Vec.fromList [GiftPairStruct {givee = "PatSha", giver = "TroBro"}]}), ("DunKei", PlayerStruct {playerName = "Duncan Keith", giftHistory = Vec.fromList [GiftPairStruct {givee = "TomKop", giver = "AdaBur"}]}), ("JoeQue", PlayerStruct {playerName = "Joel Quenneville", giftHistory = Vec.fromList [GiftPairStruct {givee = "TroBro", giver = "AndLad"}]}), ("JonToe", PlayerStruct {playerName = "Jonathan Toews", giftHistory = Vec.fromList [GiftPairStruct {givee = "AdaBur", giver = "AntNie"}]}), ("KriVer", PlayerStruct {playerName = "Kris Versteeg", giftHistory = Vec.fromList [GiftPairStruct {givee = "AndLad", giver = "BreSea"}]}), ("MarHos", PlayerStruct {playerName = "Marian Hossa", giftHistory = Vec.fromList [GiftPairStruct {givee = "AntNie", giver = "BryBic"}]}), ("NikHja", PlayerStruct {playerName = "Niklas Hjalmarsson", giftHistory = Vec.fromList [GiftPairStruct {givee = "BreSea", giver = "BriCam"}]}), ("PatKan", PlayerStruct {playerName = "Patrick Kane", giftHistory = Vec.fromList [GiftPairStruct {givee = "BryBic", giver = "CriHue"}]}), ("PatSha", PlayerStruct {playerName = "Patrick Sharp", giftHistory = Vec.fromList [GiftPairStruct {givee = "BriCam", giver = "DavBol"}]}), ("TomKop", PlayerStruct {playerName = "Tomas Kopecky", giftHistory = Vec.fromList [GiftPairStruct {givee = "CriHue", giver = "DunKei"}]}), ("TroBro", PlayerStruct {playerName = "Troy Brouwer", giftHistory = Vec.fromList [GiftPairStruct {givee = "DavBol", giver = "JoeQue"}]})], giftYear = 0, giveeHat = Set.fromList [], giverHat = Set.fromList [], maybeGivee = Nothing, maybeGiver = Nothing, discards = Set.fromList [], quit = "n"})

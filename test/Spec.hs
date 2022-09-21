-- λ> :load test/Spec.hs src/Gift_Pair.hs src/Gift_History.hs src/Player.hs src/Players
-- λ> jsonStringGiftPair
-- λ> gp1

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Gift_History
import Gift_Pair
import Player
import Players
import Roster
import Rules
import Test.Hspec

jsonStringGiftPair :: JsonString
jsonStringGiftPair = "{\"giver\":\"Giver1\",\"givee\":\"Givee1\"}"

jsonStringGiftPairBad :: JsonString
jsonStringGiftPairBad = "{\"giverX\":\"Giver1\",\"givee\":\"Givee1\"}"

giftPair1 :: GiftPair
giftPair1 = GiftPair {givee = "Givee1", giver = "Giver1"}

jsonStringGiftHistory :: JsonString
jsonStringGiftHistory = "[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]"

jsonStringGiftHistoryBad :: JsonString
jsonStringGiftHistoryBad = "[{\"giverX\":\"JohLen\",\"givee\":\"GeoHar\"}]"

giftHistory1 :: GiftHistory
giftHistory1 = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]

giftHistory2 :: GiftHistory
giftHistory2 = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "Yippee", giver = "Yippee"}]

giftHistory3 :: GiftHistory
giftHistory3 = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "Givee1", giver = "Giver1"}]

jsonStringPlayer :: JsonString
jsonStringPlayer = "{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]}"

jsonStringPlayerBad :: JsonString
jsonStringPlayerBad = "{\"playerNameX\":\"Paul McCartney\",\"giftHistory\":[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]}"

player1 :: Player
player1 = Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {giver = "JohLen", givee = "GeoHar"}]}

jsonStringPlayers :: JsonString
jsonStringPlayers = "{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}"

jsonStringPlayersBad :: JsonString
jsonStringPlayersBad = "{\"PauMcc\":{\"playerNameX\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}"

players1 :: Players
players1 =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

newBeePlayers :: Players
newBeePlayers =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "New Bee", giftHistory = Seq.fromList [GiftPair {giver = "NewBee", givee = "NewBee"}]})
    ]

playersGivee :: Players
playersGivee =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "you", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "New Bee", giftHistory = Seq.fromList [GiftPair {giver = "NewBee", givee = "NewBee"}]})
    ]

playersGiver :: Players
playersGiver =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "you"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "New Bee", giftHistory = Seq.fromList [GiftPair {giver = "NewBee", givee = "NewBee"}]})
    ]

extendedPlayers :: Players
extendedPlayers =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}, GiftPair {givee = "GeoHar", giver = "GeoHar"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}, GiftPair {givee = "JohLen", giver = "JohLen"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "PauMcc", giver = "PauMcc"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}, GiftPair {givee = "RinSta", giver = "RinSta"}]})
    ]

jsonStringRoster1 :: JsonString
jsonStringRoster1 = "{\"rosterName\":\"The Beatles\",\"rosterYear\":2014,\"players\":{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}}"

jsonStringRosterBad :: JsonString
jsonStringRosterBad = "{\"rosterNameX\":\"The Beatles\",\"rosterYear\":2014,\"players\":{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}}"

roster1 :: Roster
roster1 = Roster {rosterName = "The Beatles", rosterYear = 2014, players = players1}

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

playersP4 :: Players
playersP4 =
  let extended = playersAddYear . playersAddYear . playersAddYear $ playersAddYear playersP0
   in playersUpdateGivee "RinSta" "KarLav" 4 . playersUpdateGivee "RinSta" "EriTob" 3 . playersUpdateGivee "RinSta" "PauMcc" 2 $ playersUpdateGivee "RinSta" "GeoHar" 1 extended

-- jsonStringBorrowersBad :: String
-- jsonStringBorrowersBad =
--   "[{\"name\"\"Borrower1\",\"maxBooks\":1},{\"name\":\"Borrower2\",\"maxBooks\":2}]"

-- jsonStringBorrowers :: String
-- jsonStringBorrowers =
--   "[{\"name\":\"Borrower1\",\"maxBooks\":1},{\"name\":\"Borrower2\",\"maxBooks\":2}]"

-- jsonStringBooks :: String
-- jsonStringBooks =
--   "[{\"borrower\":{\"name\":\"Borrower1\",\"maxBooks\":1},\"title\":\"Title1\",\"author\":\"Author1\"},{\"borrower\":null,\"title\":\"Title2\",\"author\":\"Author2\"}]"

main :: IO ()
main = hspec $ do
  describe "GiftPair tests" $ do
    it "testGiftPairJsonStringToGiftPairPass" $ giftPairJsonStringToGiftPair jsonStringGiftPair `shouldBe` Just giftPair1
    it "testGiftPairJsonStringToGiftPairFail" $ giftPairJsonStringToGiftPair jsonStringGiftPairBad `shouldBe` Nothing
    it "testGiftPairGiftPairToJsonString" $ giftPairGiftPairToJsonString giftPair1 `shouldBe` jsonStringGiftPair
    it "testGiftPairUpdateGivee" $ giftPairUpdateGivee "Givee1" GiftPair {givee = "BadGivee", giver = "Giver1"} `shouldBe` giftPair1
    it "testGiftPairUpdateGiver" $ giftPairUpdateGiver "Giver1" GiftPair {givee = "Givee1", giver = "BadGiver"} `shouldBe` giftPair1

  describe "GiftHistory tests" $ do
    it "testGiftHistoryJsonStringToGiftHistoryPass" $ giftHistoryJsonStringToGiftHistory jsonStringGiftHistory `shouldBe` Just giftHistory1
    it "testGiftHistoryJsonStringToGiftHistoryFail" $ giftHistoryJsonStringToGiftHistory jsonStringGiftHistoryBad `shouldBe` Nothing
    it "testGiftHistoryGiftHistoryToJsonString" $ giftHistoryGiftHistoryToJsonString giftHistory1 `shouldBe` jsonStringGiftHistory
    it "testGiftHistoryAddYear" $ giftHistoryAddYear giftHistory1 "Yippee" `shouldBe` giftHistory2
    it "testGiftHistoryUpdateGiftHistory" $ giftHistoryUpdateGiftHistory 1 giftPair1 giftHistory2 `shouldBe` giftHistory3

  describe "Player tests" $ do
    it "testPlayerJsonStringToPlayerPass" $ playerJsonStringToPlayer jsonStringPlayer `shouldBe` Just player1
    it "testPlayerJsonStringToPlayerFail" $ playerJsonStringToPlayer jsonStringPlayerBad `shouldBe` Nothing
    it "testPlayerPlayerToJsonString" $ playerPlayerToJsonString player1 `shouldBe` jsonStringPlayer
    it "testPlayerUpdateGiftHistory" $
      playerUpdateGiftHistory giftHistory2 player1
        `shouldBe` Player
          { playerName = "Paul McCartney",
            giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "Yippee", giver = "Yippee"}]
          }

  describe "Players tests" $ do
    it "testPlayersJsonStringToPlayersPass" $ playersJsonStringToPlayers jsonStringPlayers `shouldBe` Just players1
    it "testPlayersJsonStringToPlayersFail" $ playersJsonStringToPlayers jsonStringPlayersBad `shouldBe` Nothing
    it "testPlayersUpdatePlayer" $ playersUpdatePlayer "RinSta" Player {playerName = "New Bee", giftHistory = Seq.fromList [GiftPair {giver = "NewBee", givee = "NewBee"}]} players1 `shouldBe` newBeePlayers
    it "testPlayersGetPlayerNameFound" $ playersGetPlayerName "RinSta" newBeePlayers `shouldBe` "New Bee"
    it "testPlayersGetPlayerNameNotFound" $ playersGetPlayerName "NotThere" newBeePlayers `shouldBe` "Error Finding Player"
    it "testPlayersAddYear" $ playersAddYear players1 `shouldBe` extendedPlayers
    it "testPlayersGetGiveePass" $ playersGetGivee "JohLen" players1 0 `shouldBe` "PauMcc"
    it "testPlayersGetGiveeFailPlayer" $ playersGetGivee "Nope" players1 0 `shouldBe` "Error Finding Player"
    it "testPlayersGetGiveeFailGiftYear" $ playersGetGivee "JohLen" players1 99 `shouldBe` "Error Finding GiftYear"
    it "testPlayersGetGiverPass" $ playersGetGiver "JohLen" players1 0 `shouldBe` "RinSta"
    it "testPlayersGetGiverFailPlayer" $ playersGetGiver "Nope" players1 0 `shouldBe` "Error Finding Player"
    it "testPlayersGetGiverFailGiftYear" $ playersGetGiver "JohLen" players1 99 `shouldBe` "Error Finding GiftYear"
    it "testPlayersUpdateGiveePass" $ playersUpdateGivee "GeoHar" "you" 0 newBeePlayers `shouldBe` playersGivee
    it "testPlayersUpdateGiveeFail" $ playersUpdateGivee "GeoHar" "you" 99 newBeePlayers `shouldBe` emptyPlayers
    it "testPlayersUpdateGiver" $ playersUpdateGiver "GeoHar" "you" 0 newBeePlayers `shouldBe` playersGiver
    it "testPlayersUpdateGiverFail" $ playersUpdateGiver "GeoHar" "you" 99 newBeePlayers `shouldBe` emptyPlayers

  describe "Roster tests" $ do
    it "testRosterJsonStringToRosterPass" $ rosterJsonStringToRoster jsonStringRoster1 `shouldBe` Just roster1
    it "testRosterJsonStringToRosterFail" $ rosterJsonStringToRoster jsonStringRosterBad `shouldBe` Nothing

  describe "Rules tests" $ do
    it "testRulesGiveeNotSelf" $ rulesGiveeNotSelf "JohLen" "GeoHar" `shouldBe` True
    it "testRulesGiveeNotReciprocalPass" $ rulesGiveeNotReciprocal "JohLen" players1 0 "GeoHar" `shouldBe` True
    it "testRulesGiveeNotReciprocalFail" $ rulesGiveeNotReciprocal "JohLen" reciprocalPlayers 0 "GeoHar" `shouldBe` False
    it "testRulesGiveeNotRepeatFail1" $ rulesGiveeNotRepeat "RinSta" "JohLen" 2 playersP4 `shouldBe` False
    it "testRulesGiveeNotRepeatFail2" $ rulesGiveeNotRepeat "RinSta" "GeoHar" 2 playersP4 `shouldBe` False
    it "testRulesGiveeNotRepeatPass3" $ rulesGiveeNotRepeat "RinSta" "KarLav" 2 playersP4 `shouldBe` True

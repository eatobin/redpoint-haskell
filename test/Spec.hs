-- λ> :load test/Spec.hs src/Gift_Pair.hs src/Gift_History.hs src/Player.hs
-- λ> jsonStringGiftPair
-- λ> gp1

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Gift_History
import Gift_Pair
import Player
import Players
import Test.Hspec

type JsonString = String

jsonStringGiftPair :: JsonString
jsonStringGiftPair = "{\"giver\":\"Giver1\",\"givee\":\"Givee1\"}"

giftPair1 :: GiftPair
giftPair1 = GiftPair {givee = "Givee1", giver = "Giver1"}

jsonStringGiftHistory :: JsonString
jsonStringGiftHistory = "[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]"

giftHistory1 :: Seq.Seq GiftPair
giftHistory1 = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]

giftHistory2 :: Seq.Seq GiftPair
giftHistory2 = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "Yippee", giver = "Yippee"}]

giftHistory3 :: Seq.Seq GiftPair
giftHistory3 = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "Givee1", giver = "Giver1"}]

jsonStringPlayer :: JsonString
jsonStringPlayer = "{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"giver\":\"JohLen\",\"givee\":\"GeoHar\"}]}"

player1 :: Player
player1 = Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {giver = "JohLen", givee = "GeoHar"}]}

jsonStringPlayers :: JsonString
jsonStringPlayers = "{\"PauMcc\":{\"playerName\":\"Paul McCartney\",\"giftHistory\":[{\"givee\":\"GeoHar\",\"giver\":\"JohLen\"}]},\"GeoHar\":{\"playerName\":\"George Harrison\",\"giftHistory\":[{\"givee\":\"RinSta\",\"giver\":\"PauMcc\"}]},\"JohLen\":{\"playerName\":\"John Lennon\",\"giftHistory\":[{\"givee\":\"PauMcc\",\"giver\":\"RinSta\"}]},\"RinSta\":{\"playerName\":\"Ringo Starr\",\"giftHistory\":[{\"givee\":\"JohLen\",\"giver\":\"GeoHar\"}]}}"

players1 :: Map String Player
players1 =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

-- bk3 :: Book
-- bk3 = Book "Title3" "Author3" (Just br3)

-- bk4 :: Book
-- bk4 = Book "Title4" "Author4" (Just br3)

-- bks1 :: [Book]
-- bks1 = [bk1, bk2]

-- bks2 :: [Book]
-- bks2 = [bk3, bk1, bk2]

-- bks3 :: [Book]
-- bks3 = [bk1, bk2, bk3, bk4]

-- bks5 :: [Book]
-- bks5 = [bk3, bk1, bk2]

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
    it "testGiftPairJsonStringToGiftPair" $ giftPairJsonStringToGiftPair jsonStringGiftPair `shouldBe` Just giftPair1
    it "testGiftPairGiftPairToJsonString" $ giftPairGiftPairToJsonString giftPair1 `shouldBe` jsonStringGiftPair
    it "testGiftPairUpdateGivee" $ giftPairUpdateGivee "Givee1" GiftPair {givee = "BadGivee", giver = "Giver1"} `shouldBe` giftPair1
    it "testGiftPairUpdateGiver" $ giftPairUpdateGiver "Giver1" GiftPair {givee = "Givee1", giver = "BadGiver"} `shouldBe` giftPair1

  describe "GiftHistory tests" $ do
    it "testGiftHistoryJsonStringToGiftHistory" $ giftHistoryJsonStringToGiftHistory jsonStringGiftHistory `shouldBe` Just giftHistory1
    it "testGiftHistoryGiftHistoryToJsonString" $ giftHistoryGiftHistoryToJsonString giftHistory1 `shouldBe` jsonStringGiftHistory
    it "testGiftHistoryAddYear" $ giftHistoryAddYear giftHistory1 "Yippee" `shouldBe` giftHistory2
    it "testGiftHistoryUpdateGiftHistory" $ giftHistoryUpdateGiftHistory 1 giftPair1 giftHistory2 `shouldBe` giftHistory3

  describe "Player tests" $ do
    it "testPlayerJsonStringToPlayer" $ playerJsonStringToPlayer jsonStringPlayer `shouldBe` Just player1
    it "testPlayerPlayerToJsonString" $ playerPlayerToJsonString player1 `shouldBe` jsonStringPlayer
    it "testPlayerUpdateGiftHistory" $
      playerUpdateGiftHistory giftHistory2 player1
        `shouldBe` Player
          { playerName = "Paul McCartney",
            giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}, GiftPair {givee = "Yippee", giver = "Yippee"}]
          }
  describe "Players tests" $ do
    it "testPlayersJsonStringToPlayers" $ playersJsonStringToPlayers jsonStringPlayers `shouldBe` Just players1

module Players (playersUpdatePlayer, playersGetPlayerName, playersJsonStringToPlayers) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)
import Player
import Prelude hiding (lookup)

type PlayerKey = String

type PlayerName = String

type Players = Map PlayerKey Player

type JsonString = String

playersUpdatePlayer :: PlayerKey -> Player -> Players -> Players
-- playersUpdatePlayer playerKey player players = Map.insert playerKey player players
playersUpdatePlayer = Map.insert

playersGetPlayerName :: PlayerKey -> Players -> PlayerName
playersGetPlayerName playerKey players
  | isNothing maybePlayer = "Error Finding Player"
  | otherwise = playerName (fromJust maybePlayer)
  where
    maybePlayer = Map.lookup playerKey players

--playersAddYear :: Players -> Players
--playersAddYear players =
--  where
--    []

playersJsonStringToPlayers :: JsonString -> Maybe Players
playersJsonStringToPlayers js = A.decodeStrict (BS.pack js) :: Maybe Players

--λ> [(playerKey, newPlayer) | (playerKey, player) <- [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
--                  ("JohLen", Player {playerName = "John Lennon", giftHistory = Seq.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
--                  ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Seq.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
--                  ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Seq.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
--                ]]
-- [("GeoHar",Player {playerName = "George Harrison", giftHistory = fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),("JohLen",Player {playerName = "John Lennon", giftHistory = fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),("PauMcc",Player {playerName = "Paul McCartney", giftHistory = fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),("RinSta",Player {playerName = "Ringo Starr", giftHistory = fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]

--  def playersAddYear(players: Map[String, Player]): Map[String, Player] = {
--    val newPlayers = for ((playerKey, player) <- players) yield {
--      val gh = player.giftHistory
--      val ngh = giftHistoryAddYear(playerKey, gh)
--      val nplr = playerUpdateGiftHistory(ngh, player)
--      playerKey -> nplr
--    }
--    newPlayers
--  }

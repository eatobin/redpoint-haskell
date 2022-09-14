module Players (playersUpdatePlayer, playersJsonStringToPlayers) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Player
import Prelude hiding (lookup)

type PlayerKey = String

type Players = Map PlayerKey Player

type JsonString = String

playersUpdatePlayer :: PlayerKey -> Player -> Players -> Players
-- playersUpdatePlayer playerKey player players = Map.insert playerKey player players
playersUpdatePlayer = Map.insert

--def playersUpdatePlayer(playerKey: String, player: Player, players: Map[String, Player]): Map[String, Player] =
--    players.updated(playerKey, player)

playersJsonStringToPlayers :: JsonString -> Maybe Players
playersJsonStringToPlayers js = A.decodeStrict (BS.pack js) :: Maybe Players

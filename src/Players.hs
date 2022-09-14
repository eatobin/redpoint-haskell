module Players (playersJsonStringToPlayers) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
--import qualified Data.Map.Strict as Map
import Gift_Pair
import Player
import Prelude hiding (lookup)

type Players = Map PlayerSymbol Player

type JsonString = String

--playersUpdatePlayer ::
--def playersUpdatePlayer(playerKey: String, player: Player, players: Map[String, Player]): Map[String, Player] =
--    players.updated(playerKey, player)

playersJsonStringToPlayers :: JsonString -> Maybe Players
playersJsonStringToPlayers js = A.decodeStrict (BS.pack js) :: Maybe Players

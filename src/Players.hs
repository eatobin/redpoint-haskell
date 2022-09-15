module Players (playersUpdatePlayer, playersGetPlayerName, playersAddYear, playersJsonStringToPlayers) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)
import Gift_History
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

playersAddYear :: Players -> Players
playersAddYear players =
  Map.fromList
    [ do
        let gh = giftHistory player
        let ngh = giftHistoryAddYear gh playerKey
        let nplr = playerUpdateGiftHistory ngh player
        (playerKey, nplr)
      | (playerKey, player) <- Map.toAscList players
    ]

playersJsonStringToPlayers :: JsonString -> Maybe Players
playersJsonStringToPlayers js = A.decodeStrict (BS.pack js) :: Maybe Players

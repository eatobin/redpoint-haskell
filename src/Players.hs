module Players (playersUpdatePlayer, playersGetPlayerName, playersAddYear, playersGetGivee, playersJsonStringToPlayers) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)
import Gift_History
import Player
import Prelude hiding (lookup)

type PlayerKey = String

type SelfKey = String

type PlayerName = String

type GiftYear = Int

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

--def playersGetGivee(selfKey: String, giftYear: Int, players: Map[String, Player]): String =
--    players(selfKey).giftHistory(giftYear).givee

--playersGetGivee :: SelfKey -> GiftYear -> Players -> PlayerName
--playersGetGivee selfKey giftYear players
--  | isNothing maybePlayer = "Error Finding Player"
--  | otherwise
--    | isNothing maybeGh = "Error Finding GiftYear"
--    | otherwise "yea"
--    where
--      maybePlayer = Map.lookup selfKey players
--      maybeGh = maybePlayer !? giftYear
playersGetGivee :: SelfKey -> Players -> PlayerName
playersGetGivee selfKey players =
  case Map.lookup selfKey players of
    Nothing -> "Error Finding Player"
    (Just plr) -> do
      playerName plr

--  do
--    let firstDependency = Seq.fromList ["base", "containers"] !? 0
--    case firstDependency of
--      Nothing -> print "Whoops! No dependencies!"
--      Just dep -> print "The first dependency is " ++ dep

playersJsonStringToPlayers :: JsonString -> Maybe Players
playersJsonStringToPlayers js = A.decodeStrict (BS.pack js) :: Maybe Players

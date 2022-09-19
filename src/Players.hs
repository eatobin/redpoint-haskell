module Players (playersUpdatePlayer, playersGetPlayerName, playersAddYear, playersGetGivee, playersGetGiver, playersSetGiftPair, playersUpdateGivee, playersJsonStringToPlayers) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)
import Data.Sequence as Seq
import Gift_History
import Gift_Pair
import Player
import Prelude hiding (lookup)

type PlayerKey = String

type SelfKey = String

type PlayerName = String

type GiftYear = Int

type Players = Map PlayerKey Player

type JsonString = String

emptyPlayers :: Map String Player
emptyPlayers =
  Map.fromList
    [("EmptyPlayers", Player {playerName = "EmptyPlayers", giftHistory = Seq.fromList [GiftPair {givee = "EmptyPlayers", giver = "EmptyPlayers"}]})]

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

playersGetGivee :: SelfKey -> Players -> GiftYear -> PlayerName
playersGetGivee selfKey players giftYear =
  case Map.lookup selfKey players of
    Nothing -> "Error Finding Player"
    (Just plr) -> case Seq.lookup giftYear (giftHistory plr) of
      Nothing -> "Error Finding GiftYear"
      (Just giftPair) -> givee giftPair

playersGetGiver :: SelfKey -> Players -> GiftYear -> PlayerName
playersGetGiver selfKey players giftYear =
  case Map.lookup selfKey players of
    Nothing -> "Error Finding Player"
    (Just plr) -> case Seq.lookup giftYear (giftHistory plr) of
      Nothing -> "Error Finding GiftYear"
      (Just giftPair) -> giver giftPair

playersSetGiftPair :: PlayerKey -> Players -> GiftYear -> GiftPair -> Players
playersSetGiftPair playerKey players giftYear giftPair =
  case Map.lookup playerKey players of
    Nothing -> emptyPlayers
    (Just plr) -> do
      let ngh = giftHistoryUpdateGiftHistory giftYear giftPair (giftHistory plr)
      let nplr = playerUpdateGiftHistory ngh plr
      playersUpdatePlayer playerKey nplr players

playersUpdateGivee :: PlayerKey -> Players -> Givee -> GiftYear -> Players
playersUpdateGivee playerKey players gee giftYear =
  case Map.lookup playerKey players of
    Nothing -> emptyPlayers
    (Just plr) -> case Seq.lookup giftYear (giftHistory plr) of
      Nothing -> emptyPlayers
      (Just giftPair) -> do
        let ngp = giftPairUpdateGivee gee giftPair
        playersSetGiftPair playerKey players giftYear ngp

playersJsonStringToPlayers :: JsonString -> Maybe Players
playersJsonStringToPlayers js = A.decodeStrict (BS.pack js) :: Maybe Players

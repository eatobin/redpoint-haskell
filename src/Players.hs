module Players (Players, emptyPlayers, playersUpdatePlayer, playersGetPlayerName, playersAddYear, playersGetMyGivee, playersGetMyGiver, playersSetGiftPair, playersUpdateMyGivee, playersUpdateMyGiver, playersJsonStringToPlayers) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM
import qualified Data.Sequence as Seq
import Gift_History
import Gift_Pair
import Player
import Prelude hiding (lookup)

type Players = Map.Map PlayerKey Player

emptyPlayers :: Players
emptyPlayers =
  Map.fromList
    [("EmptyPlayers", Player {playerName = "EmptyPlayers", giftHistory = Seq.fromList [GiftPair {givee = "EmptyPlayers", giver = "EmptyPlayers"}]})]

playersUpdatePlayer :: PlayerKey -> Player -> Players -> Players
-- playersUpdatePlayer playerKey player players = Map.insert playerKey player players
playersUpdatePlayer = Map.insert

--(!) :: Ord k => Map k a -> k -> a infixl 9Source#
--
--𝑂(log𝑛). Find the value at a key. Calls error when the element can not be found.
--
--fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
--fromList [(5,'a'), (3,'b')] ! 5 == 'a'

--tester :: Players -> PlayerKey -> Player
--tester players playerKey = players Map.! playerKey

playersGetPlayerName :: PlayerKey -> Players -> PlayerName
playersGetPlayerName playerKey players
  playerName player
  where
    player = players Map.! playerKey
--    where
--        player = players Map.! playerKey

--playersGetPlayerName :: PlayerKey -> Players -> PlayerName
--playersGetPlayerName playerKey players
--  | DM.isNothing maybePlayer = "Error Finding Player"
--  | otherwise = playerName (DM.fromJust maybePlayer)
--  where
--    maybePlayer = Map.lookup playerKey players

playersAddYear :: Players -> Players
playersAddYear players =
  Map.fromList
    [ do
        let gh = giftHistory player
        let ngh = giftHistoryAddYear playerKey gh
        let nplr = playerUpdateGiftHistory ngh player
        (playerKey, nplr)
      | (playerKey, player) <- Map.toAscList players
    ]

playersGetMyGivee :: PlayerKey -> Players -> GiftYear -> Givee
playersGetMyGivee selfKey players giftYear =
  case Map.lookup selfKey players of
    Nothing -> "Error Finding Player"
    (Just plr) -> case Seq.lookup giftYear (giftHistory plr) of
      Nothing -> "Error Finding GiftYear"
      (Just giftPair) -> givee giftPair

playersGetMyGiver :: PlayerKey -> Players -> GiftYear -> Giver
playersGetMyGiver selfKey players giftYear =
  case Map.lookup selfKey players of
    Nothing -> "Error Finding Player"
    (Just plr) -> case Seq.lookup giftYear (giftHistory plr) of
      Nothing -> "Error Finding GiftYear"
      (Just giftPair) -> giver giftPair

playersSetGiftPair :: PlayerKey -> GiftYear -> GiftPair -> Players -> Players
playersSetGiftPair playerKey giftYear giftPair players =
  case Map.lookup playerKey players of
    Nothing -> emptyPlayers
    (Just plr) -> do
      let ngh = giftHistoryUpdateGiftHistory giftYear giftPair (giftHistory plr)
      let nplr = playerUpdateGiftHistory ngh plr
      playersUpdatePlayer playerKey nplr players

playersUpdateMyGivee :: PlayerKey -> Givee -> GiftYear -> Players -> Players
playersUpdateMyGivee playerKey gee giftYear players =
  case Map.lookup playerKey players of
    Nothing -> emptyPlayers
    (Just plr) -> case Seq.lookup giftYear (giftHistory plr) of
      Nothing -> emptyPlayers
      (Just giftPair) -> do
        let ngp = giftPairUpdateGivee gee giftPair
        playersSetGiftPair playerKey giftYear ngp players

playersUpdateMyGiver :: PlayerKey -> Giver -> GiftYear -> Players -> Players
playersUpdateMyGiver playerKey ger giftYear players =
  case Map.lookup playerKey players of
    Nothing -> emptyPlayers
    (Just plr) -> case Seq.lookup giftYear (giftHistory plr) of
      Nothing -> emptyPlayers
      (Just giftPair) -> do
        let ngp = giftPairUpdateGiver ger giftPair
        playersSetGiftPair playerKey giftYear ngp players

playersJsonStringToPlayers :: JsonString -> Maybe Players
playersJsonStringToPlayers jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe Players

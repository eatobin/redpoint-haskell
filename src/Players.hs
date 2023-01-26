module Players (Players, playersUpdatePlayer, playersGetPlayerName, playersAddYear, playersGetMyGivee, playersGetMyGiver, playersSetGiftPair, playersUpdateMyGivee, playersUpdateMyGiver, playersJsonStringToPlayers) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import Gift_History
import Gift_Pair
import Player

type Players = Map.Map PlayerKey Player

playersUpdatePlayer :: PlayerKey -> Player -> Players -> Players
-- playersUpdatePlayer playerKey player players = Map.insert playerKey player players
playersUpdatePlayer = Map.insert

playersGetPlayerName :: PlayerKey -> Players -> PlayerName
playersGetPlayerName playerKey players =
  let player = players Map.! playerKey
   in playerName player

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
  let player = players Map.! selfKey
      giftPair = giftHistory player Vec.! giftYear
   in givee giftPair

playersGetMyGiver :: PlayerKey -> Players -> GiftYear -> Giver
playersGetMyGiver selfKey players giftYear =
  let player = players Map.! selfKey
      giftPair = giftHistory player Vec.! giftYear
   in giver giftPair

playersSetGiftPair :: PlayerKey -> GiftYear -> GiftPair -> Players -> Players
playersSetGiftPair playerKey giftYear giftPair players =
  let player = players Map.! playerKey
      ngh = giftHistoryUpdateGiftHistory giftYear giftPair (giftHistory player)
      nplr = playerUpdateGiftHistory ngh player
   in playersUpdatePlayer playerKey nplr players

playersUpdateMyGivee :: PlayerKey -> Givee -> GiftYear -> Players -> Players
playersUpdateMyGivee selfKey gee giftYear players =
  let player = players Map.! selfKey
      giftPair = giftHistory player Vec.! giftYear
      ngp = giftPairUpdateGivee gee giftPair
   in playersSetGiftPair selfKey giftYear ngp players

playersUpdateMyGiver :: PlayerKey -> Giver -> GiftYear -> Players -> Players
playersUpdateMyGiver selfKey ger giftYear players =
  let player = players Map.! selfKey
      giftPair = giftHistory player Vec.! giftYear
      ngp = giftPairUpdateGiver ger giftPair
   in playersSetGiftPair selfKey giftYear ngp players

playersJsonStringToPlayers :: JsonString -> Maybe Players
playersJsonStringToPlayers jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe Players

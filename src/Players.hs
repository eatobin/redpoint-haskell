module Players (PlayersMap, playersUpdatePlayer, playersGetPlayerName, playersAddYear, playersGetMyGivee, playersGetMyGiver, playersSetGiftPair, playersUpdateMyGivee, playersUpdateMyGiver, playersJsonStringToPlayersMap) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import GiftHistory
  ( GiftYear,
    giftHistoryAddYear,
    giftHistoryUpdateGiftHistoryVector,
  )
import GiftPair
  ( GiftPairStruct (givee, giver),
    Givee,
    Giver,
    JsonString,
    PlayerKey,
    giftPairUpdateGivee,
    giftPairUpdateGiver,
  )
import Player
  ( PlayerName,
    PlayerStruct (giftHistory, playerName),
    playerUpdateGiftHistory,
  )

type PlayersMap = Map.Map PlayerKey PlayerStruct

playersJsonStringToPlayersMap :: JsonString -> Maybe PlayersMap
playersJsonStringToPlayersMap jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe PlayersMap

playersUpdatePlayer :: PlayerKey -> PlayerStruct -> PlayersMap -> PlayersMap
-- playersUpdatePlayer playerKey player players = Map.insert playerKey player players
playersUpdatePlayer = Map.insert

playersGetPlayerName :: PlayerKey -> PlayersMap -> PlayerName
playersGetPlayerName playerKey players =
  let player = players Map.! playerKey
   in playerName player

playersAddYear :: PlayersMap -> PlayersMap
playersAddYear players =
  Map.fromList
    [ do
        let gh = giftHistory player
        let ngh = giftHistoryAddYear playerKey gh
        let nplr = playerUpdateGiftHistory ngh player
        (playerKey, nplr)
      | (playerKey, player) <- Map.toAscList players
    ]

playersGetMyGivee :: PlayerKey -> PlayersMap -> GiftYear -> Givee
playersGetMyGivee selfKey players giftYear =
  let player = players Map.! selfKey
      giftPair = giftHistory player Vec.! giftYear
   in givee giftPair

playersGetMyGiver :: PlayerKey -> PlayersMap -> GiftYear -> Giver
playersGetMyGiver selfKey players giftYear =
  let player = players Map.! selfKey
      giftPair = giftHistory player Vec.! giftYear
   in giver giftPair

playersSetGiftPair :: PlayerKey -> GiftYear -> GiftPairStruct -> PlayersMap -> PlayersMap
playersSetGiftPair playerKey giftYear giftPair players =
  let player = players Map.! playerKey
      ngh = giftHistoryUpdateGiftHistoryVector giftYear giftPair (giftHistory player)
      nplr = playerUpdateGiftHistory ngh player
   in playersUpdatePlayer playerKey nplr players

playersUpdateMyGivee :: PlayerKey -> Givee -> GiftYear -> PlayersMap -> PlayersMap
playersUpdateMyGivee selfKey gee giftYear players =
  let player = players Map.! selfKey
      giftPair = giftHistory player Vec.! giftYear
      ngp = giftPairUpdateGivee gee giftPair
   in playersSetGiftPair selfKey giftYear ngp players

playersUpdateMyGiver :: PlayerKey -> Giver -> GiftYear -> PlayersMap -> PlayersMap
playersUpdateMyGiver selfKey ger giftYear players =
  let player = players Map.! selfKey
      giftPair = giftHistory player Vec.! giftYear
      ngp = giftPairUpdateGiver ger giftPair
   in playersSetGiftPair selfKey giftYear ngp players

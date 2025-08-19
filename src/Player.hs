{-# LANGUAGE DeriveGeneric #-}

module Player (PlayerName, PlayerStruct (..), playerUpdateGiftHistory, playerJsonStringToPlayerStruct) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified GHC.Generics as G
import GiftHistory (GiftHistoryVector)
import GiftPair (JsonString)

type PlayerName = String

data PlayerStruct = PlayerStruct
  { playerName :: PlayerName,
    giftHistory :: GiftHistoryVector
  }
  deriving (Show, Eq, G.Generic)

instance A.FromJSON PlayerStruct

playerJsonStringToPlayerStruct :: JsonString -> Maybe PlayerStruct
playerJsonStringToPlayerStruct jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe PlayerStruct

playerUpdateGiftHistory :: GiftHistoryVector -> PlayerStruct -> PlayerStruct
playerUpdateGiftHistory giftHistory1 player = player {giftHistory = giftHistory1}

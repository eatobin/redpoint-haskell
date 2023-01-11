{-# LANGUAGE DeriveGeneric #-}

module Player (PlayerName, Player (..), playerUpdateGiftHistory, playerJsonStringToPlayer) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified GHC.Generics as G
import Gift_History
import Gift_Pair

type PlayerName = String

data Player = Player
  { playerName :: PlayerName,
    giftHistory :: GiftHistory
  }
  deriving (Show, Eq, G.Generic)

instance A.FromJSON Player

instance A.ToJSON Player

playerUpdateGiftHistory :: GiftHistory -> Player -> Player
playerUpdateGiftHistory giftHistory1 player = player {giftHistory = giftHistory1}

playerJsonStringToPlayer :: JsonString -> Maybe Player
playerJsonStringToPlayer jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe Player

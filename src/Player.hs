{-# LANGUAGE DeriveGeneric #-}

module Player (PlayerName, Player (..), playerUpdateGiftHistory, playerJsonStringToPlayer, playerPlayerToJsonString) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Gift_History
import Gift_Pair

type PlayerName = String

data Player = Player
  { playerName :: PlayerName,
    giftHistory :: GiftHistory
  }
  deriving (Show, Eq, Generic)

instance FromJSON Player

instance ToJSON Player

playerUpdateGiftHistory :: GiftHistory -> Player -> Player
playerUpdateGiftHistory gh player = player {giftHistory = gh}

playerJsonStringToPlayer :: JsonString -> Maybe Player
playerJsonStringToPlayer js = A.decodeStrict (BS.pack js) :: Maybe Player

playerPlayerToJsonString :: Player -> JsonString
playerPlayerToJsonString plr = BS.unpack (BL.toStrict $ A.encode plr)

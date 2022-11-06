{-# LANGUAGE DeriveGeneric #-}

module Gift_Pair (PlayerSymbol, Givee, Giver, JsonString, GiftPair (..), giftPairUpdateGivee, giftPairUpdateGiver, giftPairJsonStringToGiftPair, giftPairGiftPairToJsonString) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified GHC.Generics as G

type PlayerSymbol = String

type Givee = PlayerSymbol

type Giver = PlayerSymbol

type JsonString = String

data GiftPair = GiftPair
  { givee :: Givee,
    giver :: Giver
  }
  deriving (Show, Eq, G.Generic)

instance FromJSON GiftPair

instance ToJSON GiftPair

giftPairUpdateGivee :: Givee -> GiftPair -> GiftPair
giftPairUpdateGivee givee1 giftPair = giftPair {givee = givee1}

giftPairUpdateGiver :: Giver -> GiftPair -> GiftPair
giftPairUpdateGiver giver1 giftPair = giftPair {giver = giver1}

giftPairJsonStringToGiftPair :: JsonString -> Maybe GiftPair
giftPairJsonStringToGiftPair jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe GiftPair

giftPairGiftPairToJsonString :: GiftPair -> JsonString
giftPairGiftPairToJsonString giftPair = BS.unpack (BL.toStrict $ A.encode giftPair)

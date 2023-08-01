{-# LANGUAGE DeriveGeneric #-}

module Gift_Pair (PlayerKey, Givee, Giver, JsonString, GiftPair (..), giftPairUpdateGivee, giftPairUpdateGiver, giftPairJsonStringToGiftPair) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified GHC.Generics as G

type PlayerKey = String

type Givee = PlayerKey

type Giver = PlayerKey

type JsonString = String

data GiftPair = GiftPair
  { givee :: !Givee,
    giver :: !Giver
  }
  deriving (Show, Eq, G.Generic)

instance A.FromJSON GiftPair

giftPairJsonStringToGiftPair :: JsonString -> Maybe GiftPair
giftPairJsonStringToGiftPair jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe GiftPair

giftPairUpdateGivee :: Givee -> GiftPair -> GiftPair
giftPairUpdateGivee givee1 giftPair = giftPair {givee = givee1}

giftPairUpdateGiver :: Giver -> GiftPair -> GiftPair
giftPairUpdateGiver giver1 giftPair = giftPair {giver = giver1}

{-# LANGUAGE DeriveGeneric #-}

module GiftPair (PlayerKey, Givee, Giver, JsonString, GiftPairStruct (..), giftPairUpdateGivee, giftPairUpdateGiver, giftPairJsonStringToGiftPairStruct) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified GHC.Generics as G

type PlayerKey = String

type Givee = PlayerKey

type Giver = PlayerKey

type JsonString = String

data GiftPairStruct = GiftPairStruct
  { givee :: Givee,
    giver :: Giver
  }
  deriving (Show, Eq, G.Generic)

instance A.FromJSON GiftPairStruct

giftPairJsonStringToGiftPairStruct :: JsonString -> Maybe GiftPairStruct
giftPairJsonStringToGiftPairStruct jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe GiftPairStruct

giftPairUpdateGivee :: Givee -> GiftPairStruct -> GiftPairStruct
giftPairUpdateGivee givee1 giftPair = giftPair {givee = givee1}

giftPairUpdateGiver :: Giver -> GiftPairStruct -> GiftPairStruct
giftPairUpdateGiver giver1 giftPair = giftPair {giver = giver1}

-- λ> gp = GiftPair "gee" "ger"
-- λ> givee gp
-- "gee"
--λ> giver gp
-- "ger"
{-# LANGUAGE DeriveGeneric #-}

module Gift_Pair (PlayerSymbol, Givee, Giver, JsonString, GiftPair (..), giftPairUpdateGivee, giftPairUpdateGiver, giftPairJsonStringToGiftPair, giftPairGiftPairToJsonString) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import GHC.Generics

type PlayerSymbol = String

type Givee = PlayerSymbol

type Giver = PlayerSymbol

type JsonString = String

data GiftPair = GiftPair
  { givee :: Givee,
    giver :: Giver
  }
  deriving (Show, Eq, Generic)

instance FromJSON GiftPair

instance ToJSON GiftPair

giftPairUpdateGivee :: Givee -> GiftPair -> GiftPair
giftPairUpdateGivee gee giftPair = giftPair {givee = gee}

giftPairUpdateGiver :: Giver -> GiftPair -> GiftPair
giftPairUpdateGiver ger giftPair = giftPair {giver = ger}

giftPairJsonStringToGiftPair :: JsonString -> Maybe GiftPair
giftPairJsonStringToGiftPair js = A.decodeStrict (BS.pack js) :: Maybe GiftPair

giftPairGiftPairToJsonString :: GiftPair -> JsonString
giftPairGiftPairToJsonString gp = BS.unpack (BL.toStrict $ A.encode gp)

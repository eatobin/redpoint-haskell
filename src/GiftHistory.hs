module GiftHistory (GiftHistoryVector, GiftYear, giftHistoryAddYear, giftHistoryUpdateGiftHistoryVector, giftHistoryJsonStringToGiftHistoryVector) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as Vec
import GiftPair

type GiftHistoryVector = Vec.Vector GiftPairStruct

type GiftYear = Int

giftHistoryJsonStringToGiftHistoryVector :: JsonString -> Maybe GiftHistoryVector
giftHistoryJsonStringToGiftHistoryVector jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe GiftHistoryVector

giftHistoryAddYear :: PlayerKey -> GiftHistoryVector -> GiftHistoryVector
giftHistoryAddYear playerKey giftHistoryVector = Vec.snoc giftHistoryVector (GiftPairStruct {givee = playerKey, giver = playerKey})

giftHistoryUpdateGiftHistoryVector :: GiftYear -> GiftPairStruct -> GiftHistoryVector -> GiftHistoryVector
giftHistoryUpdateGiftHistoryVector giftYear giftPair giftHistoryVector = giftHistoryVector Vec.// [(giftYear, giftPair)]

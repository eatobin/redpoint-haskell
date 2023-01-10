module Gift_History (GiftHistory, GiftYear, giftHistoryAddYear, giftHistoryUpdateGiftHistory, giftHistoryJsonStringToGiftHistory) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as Vec
import Gift_Pair

type GiftHistory = Vec.Vector GiftPair

type GiftYear = Int

giftHistoryAddYear :: PlayerKey -> GiftHistory -> GiftHistory
giftHistoryAddYear playerKey giftHistory = Vec.snoc giftHistory (GiftPair {givee = playerKey, giver = playerKey})

giftHistoryUpdateGiftHistory :: GiftYear -> GiftPair -> GiftHistory -> GiftHistory
giftHistoryUpdateGiftHistory giftYear giftPair giftHistory = giftHistory Vec.// [(giftYear, giftPair)]

giftHistoryJsonStringToGiftHistory :: JsonString -> Maybe GiftHistory
giftHistoryJsonStringToGiftHistory jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe GiftHistory

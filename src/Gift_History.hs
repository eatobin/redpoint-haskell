module Gift_History (GiftHistory, GiftYear, giftHistoryAddYear, giftHistoryUpdateGiftHistory, giftHistoryJsonStringToGiftHistory) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence as Seq
import Gift_Pair

type GiftHistory = Seq.Seq GiftPair

type GiftYear = Int

giftHistoryAddYear :: PlayerKey -> GiftHistory -> GiftHistory
giftHistoryAddYear playerKey giftHistory = giftHistory Seq.|> (GiftPair {givee = playerKey, giver = playerKey})

giftHistoryUpdateGiftHistory :: GiftYear -> GiftPair -> GiftHistory -> GiftHistory
-- giftHistoryUpdateGiftHistory giftYear giftPair giftHistory = Seq.update giftYear giftPair giftHistory
-- or in ETA reduction (https://sookocheff.com/post/fp/eta-conversion/):
giftHistoryUpdateGiftHistory = Seq.update

giftHistoryJsonStringToGiftHistory :: JsonString -> Maybe GiftHistory
giftHistoryJsonStringToGiftHistory jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe GiftHistory

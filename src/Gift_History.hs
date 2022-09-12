module Gift_History (giftHistoryAddYear, giftHistoryUpdateGiftHistory, giftHistoryJsonStringToGiftHistory, giftHistoryGiftHistoryToJsonString) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Gift_Pair

type GiftHistory = Seq GiftPair

type PlayerKey = String

type GiftYear = Int

type JsonString = String

giftHistoryAddYear :: GiftHistory -> PlayerKey -> GiftHistory
giftHistoryAddYear giftHistory playerKey = giftHistory |> (GiftPair {givee = playerKey, giver = playerKey})

giftHistoryUpdateGiftHistory :: GiftYear -> GiftPair -> GiftHistory -> GiftHistory
-- giftHistoryUpdateGiftHistory giftYear giftPair giftHistory = Seq.update giftYear giftPair giftHistory
-- or in ETA reduction (https://sookocheff.com/post/fp/eta-conversion/):
giftHistoryUpdateGiftHistory = Seq.update

giftHistoryJsonStringToGiftHistory :: JsonString -> Maybe GiftHistory
giftHistoryJsonStringToGiftHistory js = A.decodeStrict (BS.pack js) :: Maybe GiftHistory

giftHistoryGiftHistoryToJsonString :: GiftHistory -> JsonString
giftHistoryGiftHistoryToJsonString gh = BS.unpack (BL.toStrict $ A.encode gh)

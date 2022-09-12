module Gift_History (giftHistoryJsonStringToGiftHistory, giftHistoryGiftHistoryToJsonString, giftHistoryAddYear) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Sequence (Seq (..), (|>))
import Gift_Pair

type GiftHistory = Seq GiftPair

type JsonString = String

type PlayerKey = String

giftHistoryAddYear :: GiftHistory -> PlayerKey -> GiftHistory
giftHistoryAddYear giftHistory playerKey = giftHistory |> (GiftPair {givee = playerKey, giver = playerKey})



giftHistoryJsonStringToGiftHistory :: JsonString -> Maybe GiftHistory
giftHistoryJsonStringToGiftHistory js = A.decodeStrict (BS.pack js) :: Maybe GiftHistory

giftHistoryGiftHistoryToJsonString :: GiftHistory -> JsonString
giftHistoryGiftHistoryToJsonString gh = BS.unpack (BL.toStrict $ A.encode gh)

module Gift_History (giftHistoryJsonStringToGiftHistory, giftHistoryGiftHistoryToJsonString) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence as Seq
import Gift_Pair

type GiftHistory = Seq.Seq GiftPair

type JsonString = String

--giftPairUpdateGivee :: Givee -> GiftPair -> GiftPair
--giftPairUpdateGivee gee giftPair = giftPair {givee = gee}

--giftPairUpdateGiver :: Giver -> GiftPair -> GiftPair
--giftPairUpdateGiver ger giftPair = giftPair {giver = ger}

giftHistoryJsonStringToGiftHistory :: JsonString -> Maybe GiftHistory
giftHistoryJsonStringToGiftHistory js = A.decodeStrict (BS.pack js) :: Maybe GiftHistory

giftHistoryGiftHistoryToJsonString :: GiftHistory -> JsonString
giftHistoryGiftHistoryToJsonString gh = BS.unpack (BL.toStrict $ A.encode gh)

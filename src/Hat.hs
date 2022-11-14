module Hat (Hat, Discards, hatMakeHat, hatRemovePuck, hatDiscardGivee, hatReturnDiscards, hatJsonStringToHat, hatHatToJsonString) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Gift_Pair
import Players

type Hat = Set.Set PlayerKey

type Discards = Set.Set PlayerKey

hatMakeHat :: Players -> Hat
hatMakeHat = Map.keysSet

hatRemovePuck :: PlayerKey -> Hat -> Hat
hatRemovePuck = Set.delete

hatDiscardGivee :: Givee -> Discards -> Discards
hatDiscardGivee = Set.insert

hatReturnDiscards :: Discards -> Hat -> Hat
hatReturnDiscards = Set.union

hatJsonStringToHat :: JsonString -> Maybe Hat
hatJsonStringToHat jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe Hat

hatHatToJsonString :: Hat -> JsonString
hatHatToJsonString hat = BS.unpack (BL.toStrict $ A.encode hat)

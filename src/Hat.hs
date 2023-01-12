module Hat (Hat, Discards, hatMakeHat, hatRemovePuck, hatDiscardGivee, hatReturnDiscards, hatJsonStringToHat) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Gift_Pair
import Players

type Hat = Set.Set PlayerKey

type Discards = Hat

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

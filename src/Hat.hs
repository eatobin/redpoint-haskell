module Hat (HatSet, DiscardsSet, hatMakeHat, hatRemovePuck, hatDiscardGivee, hatReturnDiscards, hatJsonStringToHatSet) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GiftPair (Givee, JsonString, PlayerKey)
import Players (PlayersMap)

type HatSet = Set.Set PlayerKey

type DiscardsSet = HatSet

hatJsonStringToHatSet :: JsonString -> Maybe HatSet
hatJsonStringToHatSet jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe HatSet

hatMakeHat :: PlayersMap -> HatSet
hatMakeHat = Map.keysSet

hatRemovePuck :: PlayerKey -> HatSet -> HatSet
hatRemovePuck = Set.delete

hatDiscardGivee :: Givee -> DiscardsSet -> DiscardsSet
hatDiscardGivee = Set.insert

hatReturnDiscards :: DiscardsSet -> HatSet -> HatSet
hatReturnDiscards = Set.union

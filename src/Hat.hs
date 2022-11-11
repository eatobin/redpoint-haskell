module Hat (Hat, Discards, hatMakeHat, hatRemovePuck, hatDiscardGivee, hatReturnDiscards) where

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

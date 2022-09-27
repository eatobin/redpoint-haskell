module Hat (Hat, Discards, hatMakeHat, hatRemovePuck, hatDiscardGivee, hatReturnDiscards) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Gift_Pair
import Players

type Hat = Set.Set PlayerSymbol

type Discards = Set.Set PlayerSymbol

hatMakeHat :: Players -> Hat
hatMakeHat = Map.keysSet

hatRemovePuck :: PlayerSymbol -> Hat -> Hat
hatRemovePuck = Set.delete

hatDiscardGivee :: Givee -> Discards -> Discards
hatDiscardGivee = Set.insert

hatReturnDiscards :: Discards -> Hat -> Hat
hatReturnDiscards = Set.union
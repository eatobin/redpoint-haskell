module Hat (Hat, Discards, hatMakeHat) where

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Gift_Pair
import Players

type Hat = Set PlayerSymbol

type Discards = Set PlayerSymbol

hatMakeHat :: Players -> Hat
hatMakeHat = Map.keysSet

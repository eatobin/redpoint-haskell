module Hat (Hat, Discards) where

import Data.Set (Set)
import Gift_Pair

type Hat = Set PlayerSymbol

type Discards = Set PlayerSymbol

module Hat where

import           Roster
import           Roster_Utility
import qualified Data.Map.Strict        as Map
import Data.Random.Extras

type Hat = [PlrSym]

makeHat :: PlayersMap -> Hat
makeHat pm = Map.keys pm

drawPuck :: Hat -> PlrSym
drawPuck h = 
  if not (null h)
    then sample 1 h
    else "empty"

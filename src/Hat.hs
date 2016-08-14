module Hat where

import qualified Data.Map.Strict as Map
import           Roster
import           Roster_Utility
import           System.Random

type Hat = [PlrSym]

makeHat :: PlayersMap -> Hat
makeHat = Map.keys

drawPuck :: Hat -> IO PlrSym
drawPuck h =
 fmap (h !!) (randomRIO (0, length h - 1))

removePuck :: PlrSym -> Hat -> Hat
removePuck ps h =
  filter (\p -> p /= ps) h

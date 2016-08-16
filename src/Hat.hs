module Hat where

import qualified Data.Map.Strict as Map
import           Roster
import           Roster_Utility
import           System.Random

type Hat = [PlrSym]
type Discards = [PlrSym]

makeHat :: PlayersMap -> Hat
makeHat = Map.keys

drawPuck :: Hat -> IO PlrSym
drawPuck h =
 fmap (h !!) (randomRIO (0, length h - 1))

removePuck :: PlrSym -> Hat -> Hat
removePuck ps =
  filter (/= ps)

discardPuck :: PlrSym -> Discards -> Discards
discardPuck ps d =
  ps : d

returnDiscards :: Discards -> Hat -> Hat
returnDiscards d h =
  h ++ d

emptyDiscards :: Discards -> Discards
emptyDiscards d = []

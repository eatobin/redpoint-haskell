module Hat where

import qualified Data.Map.Strict as Map
import           Roster
import           Roster_Utility

type GiveeHat = [Givee]
type GiverHat = [Giver]
type Discards = [Givee]

makeHatGivee :: PlayersMap -> GiveeHat
makeHatGivee = Map.keys

makeHatGiver :: PlayersMap -> GiverHat
makeHatGiver = Map.keys

removePuckGivee :: Givee -> GiveeHat -> GiveeHat
removePuckGivee ge =
  filter (/= ge)

removePuckGiver :: Giver -> GiverHat -> GiverHat
removePuckGiver gr =
  filter (/= gr)

discardPuckGivee :: Givee -> Discards -> Discards
discardPuckGivee ge d =
  d ++ [ge]

returnDiscards :: Discards -> GiveeHat -> GiveeHat
returnDiscards d geh =
  geh ++ d

emptyDiscards :: Discards -> Discards
emptyDiscards d = []

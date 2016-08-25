module Hat where

import qualified Data.Map.Strict as Map
import           Roster
import           Roster_Utility
import           System.Random

type GiveeHat = [Givee]
type GiverHat = [Giver]
type Discards = [Givee]

makeHatGivee :: PlayersMap -> GiveeHat
makeHatGivee = Map.keys

makeHatGiver :: PlayersMap -> GiverHat
makeHatGiver = Map.keys

drawPuckGivee :: GiveeHat -> IO Givee
drawPuckGivee geh =
  if null geh
    then
      return "empty"
    else
      fmap (geh !!) (randomRIO (0, length geh -1))

drawPuckGiver :: GiverHat -> IO Giver
drawPuckGiver grh =
  if null grh
    then
      return "empty"
    else
      fmap (grh !!) (randomRIO (0, length grh -1))

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

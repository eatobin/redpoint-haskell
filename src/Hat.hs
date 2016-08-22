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

drawPuckGivee :: GiveeHat -> IO PlrSym
drawPuckGivee geh =
  fmap (geh !!) (randomRIO (0, length geh - 1))

drawPuckGiver :: GiverHat -> IO PlrSym
drawPuckGiver grh =
  fmap (grh !!) (randomRIO (0, length grh - 1))

removePuckGivee :: PlrSym -> GiveeHat -> GiveeHat
removePuckGivee ps =
  filter (/= ps)

removePuckGiver :: PlrSym -> GiverHat -> GiverHat
removePuckGiver ps =
  filter (/= ps)

discardPuckGivee :: PlrSym -> Discards -> Discards
discardPuckGivee ps d =
  d ++ [ps]

returnDiscards :: Discards -> GiveeHat -> GiveeHat
returnDiscards d geh =
  geh ++ d

emptyDiscards :: Discards -> Discards
emptyDiscards d = []

module Hat where

import           Roster
import           Roster_Utility
import qualified Data.Map.Strict        as Map
import Data.Random

type Hat = [PlrSym]

makeHat :: PlayersMap -> Hat
makeHat pm = Map.keys pm

--drawPuck :: Hat -> PlrSym
--drawPuck h = 
--  if not (null h)
--    then runRVar (randomElement h)
--    else "empty"
reversed = randomElement [0,1,2]

test = do
  x <- reversed
  return x

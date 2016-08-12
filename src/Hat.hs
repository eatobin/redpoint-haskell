module Hat where

import           Roster
import           Roster_Utility
import qualified Data.Map.Strict        as Map
import System.Random

type Hat = [PlrSym]

makeHat :: PlayersMap -> Hat
makeHat pm = Map.keys pm

--drawPuck :: Hat -> PlrSym
--drawPuck h = 
--  if not (null h)
--    then runRVar (randomElement h)
--    else "empty"
--reversed = randomElement [0,1,2]

--test = do
--  x <- reversed
--  return x
pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

randomInt :: IO Int
randomInt = pick [0,1,2]

randomChar :: IO Char
randomChar = pick ['a'..'z']

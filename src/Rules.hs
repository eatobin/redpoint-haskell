module Rules where

import           Hat
import           Roster
import           Roster_Utility

giveeNotSelf :: Giver -> Givee -> Bool
giveeNotSelf gr ge =
  gr /= ge

giveeNotRecip :: Giver -> Givee -> GYear -> PlayersMap -> Bool
giveeNotRecip gr ge gy pm =
  gr /= recip
   where
     recip = getGiverInRoster ge pm gy

module Rules where

import           Hat
import           Roster
import           Roster_Utility

giveeNotSelf :: PlrSym -> Givee -> Bool
giveeNotSelf ps ge =
  ps /= ge

giveeNotRecip :: PlrSym -> Givee -> GYear -> PlayersMap -> Bool
giveeNotRecip ps ge gy pm =
  ps /= recip
    where
      recip = getGiveeInRoster ge pm gy

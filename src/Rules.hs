module Rules where

import           Roster
import           Roster_Utility

giveeNotSelf :: PlrSym -> Givee -> Bool
giveeNotSelf ps ge =
  ps /= ge

giveeNotRecip :: PlrSym -> Givee -> GYear -> PlayersMap -> Bool
giveeNotRecip ps ge gy pm =
  ps /= myRecip
    where
      myRecip = getGiveeInRoster ge pm gy

giveeNotRepeat :: PlrSym -> Givee -> GYear -> PlayersMap -> Bool
giveeNotRepeat ps ge gy pm =
  let
    past = filter (>= 0) . takeWhile (>= (gy - 3)) $ iterate (subtract 1) (gy - 1)
    geY = getGiveeInRoster ps pm
    geYrs = fmap geY past
  in
    notElem ge geYrs

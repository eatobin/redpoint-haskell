module Rules (rulesGiveeNotSelf, rulesGiveeNotReciprocal) where

import Gift_History
import Gift_Pair
import Player
import Players
import Roster

rulesGiveeNotSelf :: SelfKey -> Givee -> Bool
rulesGiveeNotSelf ps ge =
  ps /= ge

rulesGiveeNotReciprocal :: Givee -> Players -> GiftYear -> SelfKey -> Bool
rulesGiveeNotReciprocal gee plrs giftYear selfKey =
  selfKey /= myReciprocal
  where
    myReciprocal = playersGetGivee gee plrs giftYear

--giveeNotRepeat :: PlrSym -> Givee -> GYear -> PlayersMap -> Bool
--giveeNotRepeat ps ge gy pm =
--  let
--    past = filter (>= 0) . takeWhile (>= (gy - 3)) $ iterate (subtract 1) (gy - 1)
--    geY = getGiveeInRoster ps pm
--    geYrs = map geY past
--  in
--    notElem ge geYrs

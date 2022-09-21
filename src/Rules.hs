module Rules (rulesGiveeNotSelf) where

import Gift_History
import Gift_Pair
import Player
import Players
import Roster

rulesGiveeNotSelf :: SelfKey -> Givee -> Bool
rulesGiveeNotSelf ps ge =
  ps /= ge

--rulesGiveeNotReciprocal :: SelfKey -> Givee -> GiftYear -> Players -> Bool
--rulesGiveeNotReciprocal selfKey givee giftYear players =
--  selfKey /= myReciprocal
--    where
--      myReciprocal = playersGetGivee(givee players giftYear)

--giveeNotRepeat :: PlrSym -> Givee -> GYear -> PlayersMap -> Bool
--giveeNotRepeat ps ge gy pm =
--  let
--    past = filter (>= 0) . takeWhile (>= (gy - 3)) $ iterate (subtract 1) (gy - 1)
--    geY = getGiveeInRoster ps pm
--    geYrs = map geY past
--  in
--    notElem ge geYrs

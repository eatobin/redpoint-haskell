{-# LANGUAGE ScopedTypeVariables #-}

module Rules (rulesGiveeNotSelf, rulesGiveeNotReciprocal, rulesGiveeNotRepeat) where

import Gift_History
import Gift_Pair
import Players

rulesGiveeNotSelf :: SelfKey -> Givee -> Bool
rulesGiveeNotSelf selfKey gee =
  selfKey /= gee

--rulesGiveeNotReciprocal :: Givee -> Players -> GiftYear -> SelfKey -> Bool
--rulesGiveeNotReciprocal gee plrs giftYear selfKey =
--  selfKey /= myReciprocal
--  where
--    myReciprocal = playersGetGivee gee plrs giftYear

rulesGiveeNotReciprocal :: SelfKey -> Givee -> Players -> GiftYear -> Bool
rulesGiveeNotReciprocal selfKey gee plrs giftYear =
  selfKey /= myReciprocal
  where
    myReciprocal = playersGetGivee gee plrs giftYear

rulesGiveeNotRepeat :: SelfKey -> Givee -> GiftYear -> Players -> Bool
rulesGiveeNotRepeat selfKey gee giftYear plrs =
  let past :: [GiftYear] = filter (>= 0) . takeWhile (>= (giftYear - 3)) $ iterate (subtract 1) (giftYear - 1)
      giveeInYear :: (GiftYear -> Givee) = playersGetGivee selfKey plrs
      giveesInYears :: [Givee] = map giveeInYear past
   in notElem gee giveesInYears

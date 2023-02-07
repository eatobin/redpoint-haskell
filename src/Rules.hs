{-# LANGUAGE ScopedTypeVariables #-}

module Rules (rulesGiveeNotSelf, rulesGiveeNotReciprocal, rulesGiveeNotRepeat) where

import Gift_History
import Gift_Pair
import Players

rulesGiveeNotSelf :: PlayerKey -> Givee -> Bool
rulesGiveeNotSelf selfKey gee =
  selfKey /= gee

rulesGiveeNotReciprocal :: PlayerKey -> Givee -> Players -> GiftYear -> Bool
rulesGiveeNotReciprocal selfKey gee plrs giftYear =
  let myReciprocal :: Givee = playersGetMyGivee gee plrs giftYear
   in selfKey /= myReciprocal

rulesGiveeNotRepeat :: PlayerKey -> Givee -> GiftYear -> Players -> Bool
rulesGiveeNotRepeat selfKey gee giftYear plrs =
  let past :: [GiftYear] = filter (>= 0) . takeWhile (>= (giftYear - 4)) $ iterate (subtract 1) (giftYear - 1)
      giveeInYear :: (GiftYear -> Givee) = playersGetMyGivee selfKey plrs
      giveesInYears :: [Givee] = map giveeInYear past
   in notElem gee giveesInYears

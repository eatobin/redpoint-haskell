{-# LANGUAGE NamedFieldPuns #-}

module Roster where

import           Data.List.Utils
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Roster_Create

type RName = String
type RYear = String


getRosterName :: RosterString -> RName
getRosterName rosterString =
  let ri = getRosterInfo rosterString
  in case ri of
    (x:_) -> x
    _     -> "None"

getRosterYear :: RosterString -> RYear
getRosterYear rosterString =
  let ri = getRosterInfo rosterString
  in case ri of
    (_:y:_) -> y
    _       -> "None"

getPlayer :: PlrSym -> Map PlrSym Player -> Player
getPlayer ps pm =
  pm ! ps

getPlayerName :: PlrSym -> Map PlrSym Player -> PName
getPlayerName ps pm =
  let player = getPlayer ps pm
  in case player of
    Player {pName} -> pName

getGiftHistory :: PlrSym -> Map PlrSym Player -> GiftHist
getGiftHistory ps pm =
  let player = getPlayer ps pm
  in case player of
    Player {giftHist} -> giftHist

--(defn get-givee-code [p-symbol year p-map]
--  (get-in p-map
--          [p-symbol :gift-history year :givee]))

getGiveeCode :: PlrSym -> GYear -> Map PlrSym Player -> Givee
getGiveeCode ps y pm =
  let hist = getGiftHistory ps pm
      giftYear = hist !! y
  in case giftYear of
    GiftPair {givee} -> givee

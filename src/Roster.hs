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

getGiftPair :: PlrSym -> GYear -> Map PlrSym Player -> GiftPair
getGiftPair ps y pm =
  let hist = getGiftHistory ps pm
  in hist !! y

getGiveeCode :: PlrSym -> GYear -> Map PlrSym Player -> Givee
getGiveeCode ps y pm =
  let giftPair = getGiftPair ps y pm
  in case giftPair of
    GiftPair {givee} -> givee

getGiverCode :: PlrSym -> GYear -> Map PlrSym Player -> Giver
getGiverCode ps y pm =
  let giftPair = getGiftPair ps y pm
  in case giftPair of
    GiftPair {giver} -> giver

--(defn set-givee-code [p-symbol year ge p-map]
--  (if (and (contains? p-map p-symbol)
--           (contains? p-map ge)
--           (<= (+ year 1) (count
--                            (get-in p-map
--                                    [p-symbol :gift-history]))))
--    (let [gr (get-in p-map
--                     [p-symbol :gift-history year :giver])]
--      (assoc-in p-map
--                [p-symbol :gift-history year]
--                {:giver gr :givee ge}))))



--setGiveeCode :: PlrSym -> GYear -> Givee -> Map PlrSym Player -> Map PlrSym Player
--setGiveeCode ps y ge pm =
--  let histLen = length $ getGiftHistory ps pm
--  in
--    if member ps pm && member ge pm && (y + 1) <= histLen
--      then
--        let gr = getGiverCode ps y pm
--          giftPair = getGiftPair ps y pm
--      in case giftPair of
--        gp@GiftPair {givee} -> gp {givee = ge}
--    else pm

setGiveeCode ps y ge pm =
  let histLen = length $ getGiftHistory ps pm
  in
    if Map.member ps pm &&
       Map.member ge pm
       && (y + 1) <= histLen
    then
      let player = getPlayer ps pm
      in case player of
        
    else
      Player {pName = "Ringo Starr", giftHist = [GiftPair {giver = "GeoHar", givee = "JohLen"}]}

{-# LANGUAGE NamedFieldPuns #-}

module Roster where

import           Data.List.Utils
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Roster_Create

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

extractPlayerName :: Player -> PName
extractPlayerName Player {pName} = pName

extractGiftHistory :: Player -> GiftHist
extractGiftHistory Player {giftHist} = giftHist

getPlayerName :: PlrSym -> Map PlrSym Player -> PName
getPlayerName ps pm =
  extractPlayerName $ getPlayer ps pm

getGiftHistory :: PlrSym -> Map PlrSym Player -> GiftHist
getGiftHistory ps pm =
  extractGiftHistory $ getPlayer ps pm

getGiftPair :: PlrSym -> GYear -> Map PlrSym Player -> GiftPair
getGiftPair ps y pm =
  let gh = getGiftHistory ps pm
  in Seq.index gh y

extractGiftPair :: GiftHist -> GYear -> GiftPair
extractGiftPair =
  Seq.index

getGiveeCode :: PlrSym -> GYear -> Map PlrSym Player -> Givee
getGiveeCode ps y pm =
  let gp = getGiftPair ps y pm
  in case gp of
    GiftPair {givee} -> givee

getGiverCode :: PlrSym -> GYear -> Map PlrSym Player -> Giver
getGiverCode ps y pm =
  let gp = getGiftPair ps y pm
  in case gp of
    GiftPair {giver} -> giver

setPlayerGiftHist :: GiftHist -> Player -> Player
setPlayerGiftHist gh plr@Player {giftHist} = plr {giftHist = gh}

setGiftPairGiver :: Giver -> GiftPair -> GiftPair
setGiftPairGiver gr gp@GiftPair {giver} = gp {giver = gr}

setGiftPairGivee :: Givee -> GiftPair -> GiftPair
setGiftPairGivee ge gp@GiftPair {givee} = gp {givee = ge}

setGiftHistoryGiftPair :: GYear -> GiftPair -> GiftHist -> GiftHist
setGiftHistoryGiftPair =
  Seq.update

setGiveeCodeChecked :: PlrSym -> GYear -> Givee -> Map PlrSym Player -> Map PlrSym Player
setGiveeCodeChecked ps y ge pm =
  let plr = getPlayer ps pm
      gh = extractGiftHistory plr
      gp = extractGiftPair gh y
      ngp = setGiftPairGivee ge gp
      ngh = setGiftHistoryGiftPair y ngp gh
      nplr = setPlayerGiftHist ngh plr
  in Map.insert ps nplr pm

setGiveeCode :: PlrSym -> GYear -> Givee -> Map PlrSym Player -> Map PlrSym Player
setGiveeCode ps y ge pm =
  let histLen = length $ getGiftHistory ps pm
  in
    if Map.member ps pm &&
       Map.member ge pm &&
       (y + 1) <= histLen
    then
      setGiveeCodeChecked ps y ge pm
    else
      pm

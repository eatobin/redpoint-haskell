{-# LANGUAGE NamedFieldPuns #-}

module Roster where

import           Data.List.Utils
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Roster_Create

type RName = String
type RYear = Int


getRosterName :: RosterString -> RName
getRosterName rosterString =
  let rl = makeRosterList rosterString
      ri = makeRosterInfo rl
  in case ri of
    (x:_) -> x
    _     -> "None"

getRosterYear :: RosterString -> RYear
getRosterYear rosterString =
  let rl = makeRosterList rosterString
      ri = makeRosterInfo rl
  in case ri of
    (_:y:_) -> read y
    _       -> 0

getPlayer :: PlrSym -> Map PlrSym Player -> Player
getPlayer ps pm =
  pm ! ps

getPlayerName :: PlrSym -> Map PlrSym Player -> PName
getPlayerName ps pm =
  let plr = getPlayer ps pm
  in case plr of
    Player {pName} -> pName

getGiftHistory :: Player -> GiftHist
getGiftHistory Player {giftHist} = giftHist

getGiftPair :: GiftHist -> GYear -> GiftPair
getGiftPair =
  Seq.index

getGiverCode :: GiftPair -> Giver
getGiverCode GiftPair {giver} = giver

getGiveeCodePair :: GiftPair -> Givee
getGiveeCodePair GiftPair {givee} = givee

getGiveeCode :: PlrSym -> Map PlrSym Player -> GYear -> Givee
getGiveeCode ps pm gy = getGiveeCodePair $ getGiftPair (getGiftHistory $ getPlayer ps pm) gy

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
      gh = getGiftHistory plr
      gp = getGiftPair gh y
      ngp = setGiftPairGivee ge gp
      ngh = setGiftHistoryGiftPair y ngp gh
      nplr = setPlayerGiftHist ngh plr
  in Map.insert ps nplr pm

setGiveeCode :: PlrSym -> GYear -> Givee -> Map PlrSym Player -> Map PlrSym Player
setGiveeCode ps y ge pm =
  let plr = getPlayer ps pm
      gh = getGiftHistory plr
      histLen = length gh
  in
    if Map.member ps pm &&
       Map.member ge pm &&
       (y + 1) <= histLen
    then
      setGiveeCodeChecked ps y ge pm
    else
      pm

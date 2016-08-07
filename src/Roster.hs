{-# LANGUAGE NamedFieldPuns #-}

module Roster where

import           Data.List.Utils
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Roster_Utility

type RName = String
type RYear = Int


getRosterName :: RosterLine -> RName
getRosterName (x:_) = x
getRosterName _     = "None"

getRosterYear :: RosterLine -> RYear
getRosterYear (_:y:_) = read y
getRosterYear _       = 0

getPlayerNameInRoster :: PlrSym -> Map PlrSym Player -> PName
getPlayerNameInRoster ps pm =
  let plr = getPlayerInRoster ps pm
  in case plr of
    Player {pName} -> pName

setGiftPairInRoster :: PlrSym -> GYear -> GiftPair -> Map PlrSym Player -> Map PlrSym Player
setGiftPairInRoster ps gy gp pm =
  let plr = getPlayerInRoster ps pm
      gh = getGiftHistoryInPlayer plr
      ngh = setGiftPairInGiftHistory gy gp gh
      nplr = setGiftHistoryInPlayer ngh plr
  in Map.insert ps nplr pm

getGiveeInRoster :: PlrSym -> Map PlrSym Player -> GYear -> Givee
getGiveeInRoster ps pm gy =
  getGiveeInGiftPair gp
    where gp = getGiftPairInRoster ps pm gy

getGiverInRoster :: PlrSym -> Map PlrSym Player -> GYear -> Giver
getGiverInRoster ps pm gy =
  getGiverInGiftPair gp
    where gp = getGiftPairInRoster ps pm gy

setGiveeInRoster :: PlrSym -> GYear -> Givee -> Map PlrSym Player -> Map PlrSym Player
setGiveeInRoster ps gy ge pm =
  if checkGive ps gy ge pm
  then
    let gr = getGiverInRoster ps pm gy
        gp = makeGiftPair ge gr
    in setGiftPairInRoster ps gy gp pm
  else
    pm

setGiverInRoster :: PlrSym -> GYear -> Giver -> Map PlrSym Player -> Map PlrSym Player
setGiverInRoster ps gy gr pm =
  if checkGive ps gy gr pm
  then
    let ge = getGiveeInRoster ps pm gy
        gp = makeGiftPair ge gr
    in setGiftPairInRoster ps gy gp pm
  else
    pm

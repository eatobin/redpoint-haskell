{-# LANGUAGE NamedFieldPuns #-}

module Roster where

import qualified Data.Map.Strict as Map
import           Roster_Utility

getRosterName :: RosterLine -> RName
getRosterName (x:_) = x

getRosterYear :: RosterLine -> RYear
getRosterYear (_:y:_) = read y

getPlayerNameInRoster :: PlrSym -> PlayersMap -> PName
getPlayerNameInRoster ps pm =
  let plr = getPlayerInRoster ps pm
  in case plr of
    Player {pName} -> pName

getGiveeInRoster :: PlrSym -> PlayersMap -> GYear -> Givee
getGiveeInRoster ps pm gy =
  getGiveeInGiftPair gp
    where gp = getGiftPairInRoster ps pm gy

getGiverInRoster :: PlrSym -> PlayersMap -> GYear -> Giver
getGiverInRoster ps pm gy =
  getGiverInGiftPair gp
    where gp = getGiftPairInRoster ps pm gy

setGiveeInRoster :: PlrSym -> GYear -> Givee -> PlayersMap -> PlayersMap
setGiveeInRoster ps gy ge pm =
  if checkGive ps gy ge pm
  then
    let gr = getGiverInRoster ps pm gy
        gp = makeGiftPair ge gr
    in setGiftPairInRoster ps gy gp pm
  else
    pm

setGiverInRoster :: PlrSym -> GYear -> Giver -> PlayersMap -> PlayersMap
setGiverInRoster ps gy gr pm =
  if checkGive ps gy gr pm
  then
    let ge = getGiveeInRoster ps pm gy
        gp = makeGiftPair ge gr
    in setGiftPairInRoster ps gy gp pm
  else
    pm

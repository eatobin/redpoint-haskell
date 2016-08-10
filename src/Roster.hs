{-# LANGUAGE NamedFieldPuns #-}

module Roster where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Roster_Utility


getRosterName :: RosterLine -> RName
getRosterName (x:_) = x
-- getRosterName _     = "None"

getRosterYear :: RosterLine -> RYear
getRosterYear (_:y:_) = read y
-- getRosterYear _       = 0

getPlayerNameInRoster :: PlrSym -> Map PlrSym Player -> PName
getPlayerNameInRoster ps pm =
  let plr = getPlayerInRoster ps pm
  in case plr of
    Player {pName} -> pName

-- getPlayerNameInPlayer :: Player -> PName
-- getPlayerNameInPlayer Player {pName} = pName

getGiveeInRoster :: PlrSym -> Map PlrSym Player -> GYear -> Givee
getGiveeInRoster ps pm gy =
  getGiveeInGiftPair gp
    where gp = getGiftPairInRoster ps pm gy

-- getGiveeInPlayer :: GYear -> Player -> Givee
-- getGiveeInPlayer gy plr =
--   getGiveeInGiftPair gp
--     where gh = getGiftHistoryInPlayer plr
--           gp = getGiftPairInGiftHistory gh gy

getGiverInRoster :: PlrSym -> Map PlrSym Player -> GYear -> Giver
getGiverInRoster ps pm gy =
  getGiverInGiftPair gp
    where gp = getGiftPairInRoster ps pm gy

-- getGiverInPlayer :: GYear -> Player -> Giver
-- getGiverInPlayer gy plr =
--   getGiverInGiftPair gp
--     where gh = getGiftHistoryInPlayer plr
--           gp = getGiftPairInGiftHistory gh gy

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

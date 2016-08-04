{-# LANGUAGE NamedFieldPuns #-}

module Roster where

import           Data.List.Utils
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq
import           Roster_Create

type RName = String
type RYear = Int


getRosterName :: RosterLine -> RName
getRosterName (x:_) = x
getRosterName _ = "None"

getRosterYear :: RosterLine -> RYear
getRosterYear (_:y:_) = read y
getRosterYear _ = 0

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

giverPr :: GiftPair -> Giver
giverPr GiftPair {giver} = giver

giveePr :: GiftPair -> Givee
giveePr GiftPair {givee} = givee

getGivee :: PlrSym -> Map PlrSym Player -> GYear -> Givee
getGivee ps pm gy =
  giveePr gp
    where plr = getPlayer ps pm
          gh = getGiftHistory plr
          gp = getGiftPair gh gy

setPlayer :: GiftHist -> Player -> Player
setPlayer gh plr@Player {giftHist} = plr {giftHist = gh}

setPairGiver :: Giver -> GiftPair -> GiftPair
setPairGiver gr gp@GiftPair {giver} = gp {giver = gr}

setPairGivee :: Givee -> GiftPair -> GiftPair
setPairGivee ge gp@GiftPair {givee} = gp {givee = ge}

setGiftHistory :: GYear -> GiftPair -> GiftHist -> GiftHist
setGiftHistory =
  Seq.update

checkGive :: PlrSym -> GYear -> PlrSym -> Map PlrSym Player -> Bool
checkGive ps y gv pm =
  let plr = getPlayer ps pm
      gh = getGiftHistory plr
      histLen = length gh
  in
    if Map.member ps pm &&
       Map.member gv pm &&
       (y + 1) <= histLen
    then
      True
    else
      False

setGivee :: PlrSym -> GYear -> Givee -> Map PlrSym Player -> Map PlrSym Player
setGivee ps y ge pm =
  if checkGive ps y ge pm
  then
    let plr = getPlayer ps pm
        gh = getGiftHistory plr
        gp = getGiftPair gh y
        ngp = setPairGivee ge gp
        ngh = setGiftHistory y ngp gh
        nplr = setPlayer ngh plr
    in Map.insert ps nplr pm
  else
    pm

setGiver :: PlrSym -> GYear -> Giver -> Map PlrSym Player -> Map PlrSym Player
setGiver ps y gr pm =
  if checkGive ps y gr pm
  then
    let plr = getPlayer ps pm
        gh = getGiftHistory plr
        gp = getGiftPair gh y
        ngp = setPairGiver gr gp
        ngh = setGiftHistory y ngp gh
        nplr = setPlayer ngh plr
    in Map.insert ps nplr pm
  else
    pm

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
getRosterName _     = "None"

getRosterYear :: RosterLine -> RYear
getRosterYear (_:y:_) = read y
getRosterYear _       = 0

getPlayer :: PlrSym -> Map PlrSym Player -> Player
getPlayer ps pm =
  pm ! ps

setPlayer :: GiftHist -> Player -> Player
setPlayer gh plr@Player {giftHist} = plr {giftHist = gh}

getPlayerName :: PlrSym -> Map PlrSym Player -> PName
getPlayerName ps pm =
  let plr = getPlayer ps pm
  in case plr of
    Player {pName} -> pName

getGiftHistory :: Player -> GiftHist
getGiftHistory Player {giftHist} = giftHist

giftPairFrmGH :: GiftHist -> GYear -> GiftPair
giftPairFrmGH =
  Seq.index

giveeFrmPr :: GiftPair -> Givee
giveeFrmPr GiftPair {givee} = givee

giverFrmPr :: GiftPair -> Giver
giverFrmPr GiftPair {giver} = giver

getGiftPair :: PlrSym -> Map PlrSym Player -> GYear -> GiftPair
getGiftPair ps pm =
  giftPairFrmGH gh
    where plr = getPlayer ps pm
          gh = getGiftHistory plr

setGiftPair :: PlrSym -> GYear -> GiftPair -> Map PlrSym Player -> Map PlrSym Player
setGiftPair ps gy gp pm =
  let plr = getPlayer ps pm
      gh = getGiftHistory plr
      ngh = setGiftHistory gy gp gh
      nplr = setPlayer ngh plr
  in Map.insert ps nplr pm

getGivee :: PlrSym -> Map PlrSym Player -> GYear -> Givee
getGivee ps pm gy =
  giveeFrmPr gp
    where gp = getGiftPair ps pm gy

getGiver :: PlrSym -> Map PlrSym Player -> GYear -> Giver
getGiver ps pm gy =
  giverFrmPr gp
    where gp = getGiftPair ps pm gy

checkGive :: PlrSym -> GYear -> PlrSym -> Map PlrSym Player -> Bool
checkGive ps y gv pm =
  let plr = getPlayer ps pm
      gh = getGiftHistory plr
      histLen = length gh
  in
    (Map.member ps pm &&
    Map.member gv pm &&
    (y + 1) <= histLen)

setGivee :: PlrSym -> GYear -> Givee -> Map PlrSym Player -> Map PlrSym Player
setGivee ps gy ge pm =
  if checkGive ps gy ge pm
  then
    let gr = getGiver ps pm gy
        gp = makeGiftPair ge gr
    in setGiftPair ps gy gp pm
  else
    pm

setGiver :: PlrSym -> GYear -> Giver -> Map PlrSym Player -> Map PlrSym Player
setGiver ps gy gr pm =
  if checkGive ps gy gr pm
  then
    let ge = getGivee ps pm gy
        gp = makeGiftPair ge gr
    in setGiftPair ps gy gp pm
  else
    pm

setGiftHistory :: GYear -> GiftPair -> GiftHist -> GiftHist
setGiftHistory =
  Seq.update

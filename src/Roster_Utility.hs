{-# LANGUAGE NamedFieldPuns #-}

module Roster_Utility where

import           Data.List.Utils
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as Seq


type RosterString = String
type RosterList = [[String]]
type RosterLine = [String]

type PlrSym = String
type Givee = PlrSym
type Giver = PlrSym

data GiftPair = GiftPair
  { givee :: Givee
  , giver :: Giver
  } deriving (Show, Eq)

type PName = String
type GiftHist = Seq.Seq GiftPair
type GYear = Int

data Player = Player
  { pName    :: PName
  , giftHist :: GiftHist
  } deriving (Show, Eq)

type RName = String
type RYear = Int

type PlayerKV = (PlrSym, Player)
type PlayersKVList = [(PlrSym, Player)]
type PlayersMap = Map PlrSym Player

makeRosterList :: RosterString -> RosterList
makeRosterList rosterString =
  fmap (split ", ") rosterLines
    where rosterLines = lines rosterString

makeRosterInfo :: RosterList -> RosterLine
makeRosterInfo (x:_) = x
makeRosterInfo _     = ["Is", "Empty!"]

makePlayersList :: RosterList -> RosterList
makePlayersList rl =
  case rl of
    (_:xs) -> xs
    _      -> [["Is"], ["Empty!"]]

makeGiftPair :: Givee -> Giver -> GiftPair
makeGiftPair = GiftPair

makePlayer :: PName -> GiftHist -> Player
makePlayer = Player

makePlayerKV :: RosterLine -> PlayerKV
makePlayerKV [s, pn, ge, gr] =
  (s, plr)
    where gp = makeGiftPair ge gr
          plr = makePlayer pn (Seq.singleton gp)
makePlayerKV _ =
  ("error", Player {pName = "error", giftHist = Seq.fromList [GiftPair {giver = "error", givee = "error"}]})

makePlayersKVList :: RosterList -> PlayersKVList
makePlayersKVList = fmap makePlayerKV

makePlayersMapList :: PlayersKVList -> PlayersMap
makePlayersMapList  = Map.fromList

makePlayersMap :: RosterList -> PlayersMap
makePlayersMap = makePlayersMapList . makePlayersKVList . makePlayersList

getPlayerInRoster :: PlrSym -> PlayersMap -> Player
getPlayerInRoster ps pm =
  pm ! ps

getGiftPairInRoster :: PlrSym -> PlayersMap -> GYear -> GiftPair
getGiftPairInRoster ps pm =
  getGiftPairInGiftHistory gh
    where plr = getPlayerInRoster ps pm
          gh = getGiftHistoryInPlayer plr

getGiveeInGiftPair :: GiftPair -> Givee
getGiveeInGiftPair GiftPair {givee} = givee

getGiverInGiftPair :: GiftPair -> Giver
getGiverInGiftPair GiftPair {giver} = giver

getGiftHistoryInPlayer :: Player -> GiftHist
getGiftHistoryInPlayer Player {giftHist} = giftHist

getGiftPairInGiftHistory :: GiftHist -> GYear -> GiftPair
getGiftPairInGiftHistory =
  Seq.index

setGiftPairInGiftHistory :: GYear -> GiftPair -> GiftHist -> GiftHist
setGiftPairInGiftHistory =
  Seq.update

setGiftHistoryInPlayer :: GiftHist -> Player -> Player
setGiftHistoryInPlayer gh plr = plr {giftHist = gh}

setGiftPairInRoster :: PlrSym -> GYear -> GiftPair -> PlayersMap -> PlayersMap
setGiftPairInRoster ps gy gp pm =
  let plr = getPlayerInRoster ps pm
      gh = getGiftHistoryInPlayer plr
      ngh = setGiftPairInGiftHistory gy gp gh
      nplr = setGiftHistoryInPlayer ngh plr
  in Map.insert ps nplr pm

checkGive :: PlrSym -> GYear -> PlrSym -> PlayersMap -> Bool
checkGive ps y gv pm =
  let plr = getPlayerInRoster ps pm
      gh = getGiftHistoryInPlayer plr
      histLen = length gh
  in
    (Map.member ps pm &&
    Map.member gv pm &&
    (y + 1) <= histLen)

addYearInPlayer :: Player -> Player
addYearInPlayer plr =
  let gh = getGiftHistoryInPlayer plr
      ngh = gh Seq.|> GiftPair {givee = "none", giver = "none"}
  in setGiftHistoryInPlayer ngh plr

addYearInRoster :: PlayersMap -> PlayersMap
addYearInRoster =
  Map.map addYearInPlayer

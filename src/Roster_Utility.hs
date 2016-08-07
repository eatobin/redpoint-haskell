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


makeRosterList :: RosterString -> RosterList
makeRosterList rosterString =
  map (split ", ") rosterLines
    where rosterLines = lines rosterString

makeRosterInfo :: RosterList -> RosterLine
makeRosterInfo (x:_) = x
makeRosterInfo _     = []

makePlayersList :: RosterList -> RosterList
makePlayersList (_:xs) = xs
makePlayersList _      = []

makeGiftPair :: Givee -> Giver -> GiftPair
makeGiftPair = GiftPair

makePlayer :: PName -> GiftHist -> Player
makePlayer = Player

makePlayerKV :: RosterLine -> (PlrSym, Player)
makePlayerKV [s, pn, ge, gr] =
  (s, plr)
    where gp = makeGiftPair ge gr
          plr = makePlayer pn (Seq.singleton gp)

makePlayersKVList :: RosterList -> [(PlrSym, Player)]
makePlayersKVList = map makePlayerKV

makePlayersMapList :: [(PlrSym, Player)] -> Map PlrSym Player
makePlayersMapList  = Map.fromList

makePlayersMap :: RosterList -> Map PlrSym Player
makePlayersMap = makePlayersMapList . makePlayersKVList . makePlayersList

getPlayerInRoster :: PlrSym -> Map PlrSym Player -> Player
getPlayerInRoster ps pm =
  pm ! ps

getGiftPairInRoster :: PlrSym -> Map PlrSym Player -> GYear -> GiftPair
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
setGiftHistoryInPlayer gh plr@Player {giftHist} = plr {giftHist = gh}

checkGive :: PlrSym -> GYear -> PlrSym -> Map PlrSym Player -> Bool
checkGive ps y gv pm =
  let plr = getPlayerInRoster ps pm
      gh = getGiftHistoryInPlayer plr
      histLen = length gh
  in
    (Map.member ps pm &&
    Map.member gv pm &&
    (y + 1) <= histLen)

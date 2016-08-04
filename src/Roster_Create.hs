{-# LANGUAGE NamedFieldPuns #-}

module Roster_Create where

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

makePlayersMap :: RosterString -> Map PlrSym Player
makePlayersMap = makePlayersMapList . makePlayersKVList . makePlayersList . makeRosterList

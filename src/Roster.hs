{-# LANGUAGE NamedFieldPuns #-}

module Roster where

import           Data.List.Utils
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

type PlrSym = String
type Givee = PlrSym
type Giver = PlrSym

type PlayersString = String
type PlayersList = [[String]]
type PlayerLine = [String]

data GiftPair = GiftPair
  { givee :: Givee
  , giver :: Giver
  } deriving (Show, Eq)

type PName = String
type GiftHist = [GiftPair]

data Player = Player
  { pName    :: PName
  , giftHist :: GiftHist
  } deriving (Show, Eq)

type RosterString = String
type RName = String
type Year = String

data Roster = Roster
  { tName      :: RName
  , year       :: Year
  , playersMap :: Map PlrSym Player
  } deriving (Show, Eq)

makeGiftPair :: Givee -> Giver -> GiftPair
makeGiftPair = GiftPair

makePlayer :: PName -> GiftHist -> Player
makePlayer = Player

makePlayersList :: PlayersString -> PlayersList
makePlayersList playersString = map (split ", ") playerString
  where playerString = lines playersString

makePlayerKV :: PlayerLine -> (PlrSym, Player)
makePlayerKV [s, pn, ge, gr] =
  (s, plr)
    where gp = makeGiftPair ge gr
          plr = makePlayer pn [gp]

makePlayersKVList :: PlayersList -> [(PlrSym, Player)]
makePlayersKVList = map makePlayerKV

makePlayersMap :: [(PlrSym, Player)] -> Map PlrSym Player
makePlayersMap  = Map.fromList

playersMapFromString :: PlayersString -> Map PlrSym Player
playersMapFromString = makePlayersMap . makePlayersKVList . makePlayersList

getRosterName :: RosterString -> RName
getRosterName rosterString =
  head ri
    where ri = head sLines
          sLines = map (split ", ") rosterLines
          rosterLines = lines rosterString

getRosterYear :: RosterString -> Year
getRosterYear rosterString =
  last ri
    where ri = head sLines
          sLines = map (split ", ") rosterLines
          rosterLines = lines rosterString

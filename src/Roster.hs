module Roster where

import           Data.List.Utils
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

type PlrSym = String
type Givee = PlrSym
type Giver = PlrSym

type RosterList = [[String]]

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

type RName = String
type Year = String
type RosterString = String
type RosterLine = [String]

makeRosterList :: RosterString -> RosterList
makeRosterList rosterString = map (split ", ") rosterLines
  where rosterLines = lines rosterString

getRosterInfo :: RosterString -> RosterLine
getRosterInfo rosterString = head (makeRosterList rosterString)

getRosterName :: RosterString -> RName
getRosterName rosterString =
  head (getRosterInfo rosterString)

getRosterYear :: RosterString -> Year
getRosterYear rosterString =
  last (getRosterInfo rosterString)

getPlayersList :: RosterString -> RosterList
getPlayersList rosterString = drop 1 (makeRosterList rosterString)

makePlayersKVList :: RosterList -> [(PlrSym, Player)]
makePlayersKVList = map makePlayerKV

makeGiftPair :: Givee -> Giver -> GiftPair
makeGiftPair = GiftPair

makePlayer :: PName -> GiftHist -> Player
makePlayer = Player

makePlayerKV :: RosterLine -> (PlrSym, Player)
makePlayerKV [s, pn, ge, gr] =
  (s, plr)
    where gp = makeGiftPair ge gr
          plr = makePlayer pn [gp]

makePlayersMap :: [(PlrSym, Player)] -> Map PlrSym Player
makePlayersMap  = Map.fromList

playersMapFromString :: RosterString -> Map PlrSym Player
playersMapFromString = makePlayersMap . makePlayersKVList . getPlayersList

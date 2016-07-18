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

data GiftPair = GiftPair
  { givee :: Givee
  , giver :: Giver
  } deriving (Show, Eq)

type Name = String
type GiftHist = [GiftPair]

data Player = Player
  { name     :: Name
  , giftHist :: GiftHist
  } deriving (Show, Eq)

makeGiftPair :: Givee -> Giver -> GiftPair
makeGiftPair = GiftPair

makePlayer :: Name -> GiftHist -> Player
makePlayer = Player

makePlayersList :: PlayersString -> PlayersList
makePlayersList playersString = map (split ", ") playerString
  where playerString = lines playersString

makePlayerKV :: [String] -> (PlrSym, Player)
makePlayerKV [s, n, ge, gr] =
  (s, plr)
    where gp = makeGiftPair ge gr
          plr = makePlayer n [gp]

makePlayersKVList :: PlayersList -> [(PlrSym, Player)]
makePlayersKVList = map makePlayerKV

makePlayersMap :: [(PlrSym, Player)] -> Map PlrSym Player
makePlayersMap  = Map.fromList

makeRoster :: PlayersString -> Map PlrSym Player
makeRoster = makePlayersMap . makePlayersKVList . makePlayersList

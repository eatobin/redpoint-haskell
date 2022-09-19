{-# LANGUAGE DeriveGeneric #-}

module Roster (Roster (..), rosterJsonStringToRoster) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import GHC.Generics
import Player

type RosterName = String

type PlayerKey = String

type RosterYear = Int

type Players = Map PlayerKey Player

type JsonString = String

data Roster = Roster
  { rosterName :: RosterName,
    rosterYear :: RosterYear,
    players :: Players
  }
  deriving (Show, Eq, Generic)

rosterJsonStringToRoster :: JsonString -> Maybe Players
rosterJsonStringToRoster js = A.decodeStrict (BS.pack js) :: Maybe Players

{-# LANGUAGE DeriveGeneric #-}

module Roster (RosterName, RosterYear, Roster (..), rosterJsonStringToRoster) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import GHC.Generics
import Gift_Pair
import Players

type RosterName = String

type RosterYear = Int

data Roster = Roster
  { rosterName :: RosterName,
    rosterYear :: RosterYear,
    players :: Players
  }
  deriving (Show, Eq, Generic)

instance FromJSON Roster

rosterJsonStringToRoster :: JsonString -> Maybe Roster
rosterJsonStringToRoster js = A.decodeStrict (BS.pack js) :: Maybe Roster

{-# LANGUAGE DeriveGeneric #-}

module Roster (RosterName, RosterYear, Roster (..), rosterJsonStringToRoster) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified GHC.Generics as G
import Gift_History
import Gift_Pair
import Hat
import Players

type RosterName = String

type RosterYear = Int

data Roster = Roster
  { rosterName :: RosterName,
    rosterYear :: RosterYear,
    players :: Players,
    giftYear :: GiftYear,
    giveeHat :: Hat,
    giverHat :: Hat,
    maybeGivee :: Maybe Givee,
    maybeGiver :: Maybe Giver,
    discards :: Discards
  }
  deriving (Show, Eq, G.Generic)

instance A.FromJSON Roster

instance A.ToJSON Roster

rosterJsonStringToRoster :: JsonString -> Maybe Roster
rosterJsonStringToRoster jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe Roster

{-# LANGUAGE DeriveGeneric #-}

module Roster (RosterName, RosterYear, Roster (..), rosterJsonStringToRoster, rosterRosterToJsonString) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
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
    discards :: Hat
  }
  deriving (Show, Eq, G.Generic)

instance A.FromJSON Roster

instance A.ToJSON Roster

rosterJsonStringToRoster :: JsonString -> Maybe Roster
rosterJsonStringToRoster jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe Roster

rosterRosterToJsonString :: Roster -> JsonString
rosterRosterToJsonString roster = BS.unpack (BL.toStrict $ A.encode roster)

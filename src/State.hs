{-# LANGUAGE DeriveGeneric #-}

module State (Quit, State (..), stateJsonStringToState) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified GHC.Generics as G
import Gift_History
import Gift_Pair
import Hat
import Players
import Roster

type Quit = String

data State = State
  { rosterName :: RosterName,
    rosterYear :: RosterYear,
    players :: Players,
    giftYear :: GiftYear,
    giveeHat :: Hat,
    giverHat :: Hat,
    maybeGivee :: Maybe Givee,
    maybeGiver :: Maybe Giver,
    discards :: Discards,
    quit :: Quit
  }
  deriving (Show, Eq, G.Generic)

instance A.FromJSON State

stateJsonStringToState :: JsonString -> Maybe State
stateJsonStringToState jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe State

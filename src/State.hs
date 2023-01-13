{-# LANGUAGE DeriveGeneric #-}

module State (Quit, State (..), stateDrawPuck, stateStartNewYear, stateJsonStringToState) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified GHC.Generics as G
import Gift_History
import Gift_Pair
import Hat
import Players
import Roster
import System.Random.Stateful (randomRIO)

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

stateDrawPuck :: Hat -> IO (Maybe PlayerKey)
stateDrawPuck hat =
  if Set.null hat
    then return Nothing
    else do
      i <- randomRIO (0, Prelude.length hat - 1)
      return (Just (Set.elemAt i hat))

stateStartNewYear :: State -> State
stateStartNewYear state =
  let freshHat = hatMakeHat (State.players state)
      newState =
        state
          { State.rosterName = State.rosterName state,
            State.giveeHat = freshHat
          }
   in newState

stateJsonStringToState :: JsonString -> Maybe State
stateJsonStringToState jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe State

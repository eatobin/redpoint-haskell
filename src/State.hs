{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module State (Quit, State (..), stateDrawPuck, stateStartNewYear, stateJsonStringToState) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified GHC.Generics as G
import Gift_History
import Gift_Pair
import Hat
import Players
import System.Random.Stateful (randomRIO)

type RosterName = String

type RosterYear = Int

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
      i :: Int <- randomRIO (0, Prelude.length hat - 1)
      return (Just (Set.elemAt i hat))

-- TODO finish State
stateStartNewYear :: State -> State
stateStartNewYear state =
  let freshHat = hatMakeHat (players state)
      newState =
        state
          { rosterName = rosterName state,
            rosterYear = rosterYear state,
            players = playersAddYear (players state),
            giftYear = giftYear state + 1,
            giveeHat = freshHat,
            giverHat = freshHat
          }
   in newState

stateJsonStringToState :: JsonString -> Maybe State
stateJsonStringToState jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe State
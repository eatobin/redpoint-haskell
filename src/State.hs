{-# LANGUAGE ScopedTypeVariables #-}

module State (Quit, State (..), stateDrawPuck, stateStartNewYear) where

import qualified Data.Set as Set
import Gift_History
import Gift_Pair
import Hat
import Players
import System.Random

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
    maybeGivee :: IO (Maybe Givee),
    maybeGiver :: IO (Maybe Giver),
    discards :: Discards,
    quit :: Quit
  }

stateDrawPuck :: Hat -> IO (Maybe PlayerKey)
stateDrawPuck hat =
  if Set.null hat
    then return Nothing
    else do
      i :: Int <- randomRIO (0, Prelude.length hat - 1)
      return (Just (Set.elemAt i hat))

stateStartNewYear :: IO State -> IO State
stateStartNewYear ioState = do
  state <- ioState
  let freshHat = hatMakeHat (players state)
      newState :: State =
        state
          { rosterName = rosterName state,
            rosterYear = rosterYear state,
            players = playersAddYear (players state),
            giftYear = giftYear state + 1,
            giveeHat = freshHat,
            giverHat = freshHat,
            maybeGivee = stateDrawPuck freshHat,
            maybeGiver = stateDrawPuck freshHat,
            discards = Set.empty,
            quit = quit state
          }
   in return newState

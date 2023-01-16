{-# LANGUAGE ScopedTypeVariables #-}

module State (RosterName, RosterYear, Quit, State (..), stateDrawPuck, stateStartNewYear) where

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
    maybeGivee :: Maybe Givee,
    maybeGiver :: Maybe Giver,
    discards :: Discards,
    quit :: Quit
  }
  deriving (Show, Eq)

stateDrawPuck :: Hat -> IO (Maybe PlayerKey)
stateDrawPuck hat
  | Set.null hat = return Nothing
  | otherwise = do
    i :: Int <- randomRIO (0, Prelude.length hat - 1)
    return (Just (Set.elemAt i hat))

stateStartNewYear :: State -> IO State
stateStartNewYear state = do
  let freshHat = hatMakeHat (players state)
   in do
        newGivee <- stateDrawPuck freshHat
        newGiver <- stateDrawPuck freshHat
        return
          state
            { rosterName = rosterName state,
              rosterYear = rosterYear state,
              players = playersAddYear (players state),
              giftYear = giftYear state + 1,
              giveeHat = freshHat,
              giverHat = freshHat,
              maybeGivee = newGivee,
              maybeGiver = newGiver,
              discards = Set.empty,
              quit = quit state
            }

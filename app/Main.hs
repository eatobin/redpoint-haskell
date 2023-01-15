{-# LANGUAGE ScopedTypeVariables #-}

module Main (RosterName, RosterYear, Quit, main, State (..), mainDrawPuck, mainStartNewYear) where

--import Gift_History

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Gift_Pair
import Hat
import Player
import Players
import System.Random

type RosterName = String

type RosterYear = Int

type Quit = String

data State = State
  { players :: Players,
    maybeGivee :: Maybe Givee
  }
  deriving (Show, Eq)

mainDrawPuck :: Hat -> IO (Maybe PlayerKey)
mainDrawPuck hat
  | Set.null hat = return Nothing
  | otherwise = do
    i :: Int <- randomRIO (0, Prelude.length hat - 1)
    return (Just (Set.elemAt i hat))

mainStartNewYear :: State -> IO State
mainStartNewYear state = do
  newPlrKy <- mainDrawPuck (hatMakeHat (players state))
  return (state {maybeGivee = newPlrKy})

stateSpecPlayers :: Players
stateSpecPlayers =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

beatlesState :: State
beatlesState =
  State
    { players = stateSpecPlayers,
      maybeGivee = Nothing
    }

main :: IO ()
main = do
  print beatlesState

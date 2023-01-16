{-# LANGUAGE ScopedTypeVariables #-}

module Main (RosterName, RosterYear, Quit, State (..), mainDrawPuck, mainStartNewYear, mainAskContinue, main) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Gift_History
import Gift_Pair
import Hat
import Player
import Players
import System.IO (stdout)
import qualified System.IO as SIO
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

mainDrawPuck :: Hat -> IO (Maybe PlayerKey)
mainDrawPuck hat
  | Set.null hat = return Nothing
  | otherwise = do
    i :: Int <- randomRIO (0, Prelude.length hat - 1)
    return (Just (Set.elemAt i hat))

mainStartNewYear :: IO State -> IO State
mainStartNewYear ioState = do
  state <- ioState
  let freshHat = hatMakeHat (players state)
   in do
        newGivee <- mainDrawPuck freshHat
        newGiver <- mainDrawPuck freshHat
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

mainAskContinue :: State -> IO State
mainAskContinue state = do
  putStrLn ""
  putStr "\nContinue? ('q' to quit): "
  SIO.hFlush stdout
  reply <- getLine
  return state {quit = reply}

mainPlayers :: Players
mainPlayers =
  Map.fromList
    [ ("GeoHar", Player {playerName = "George Harrison", giftHistory = Vec.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]}),
      ("JohLen", Player {playerName = "John Lennon", giftHistory = Vec.fromList [GiftPair {givee = "PauMcc", giver = "RinSta"}]}),
      ("PauMcc", Player {playerName = "Paul McCartney", giftHistory = Vec.fromList [GiftPair {givee = "GeoHar", giver = "JohLen"}]}),
      ("RinSta", Player {playerName = "Ringo Starr", giftHistory = Vec.fromList [GiftPair {givee = "JohLen", giver = "GeoHar"}]})
    ]

mainBeatlesState :: State
mainBeatlesState =
  State
    { rosterName = "The Beatles",
      rosterYear = 2014,
      players = mainPlayers,
      giftYear = 0,
      giveeHat = Set.empty,
      giverHat = Set.empty,
      maybeGivee = Nothing,
      maybeGiver = Nothing,
      discards = Set.empty,
      quit = "n"
    }

main :: IO ()
main = do
  state <- mainStartNewYear (mainAskContinue mainBeatlesState)
  print state

{-# LANGUAGE ScopedTypeVariables #-}

module Main (RosterName, RosterYear, Quit, State (..), mainPrintResults, mainSelectNewGiver, mainGiveeIsSuccess, mainDrawPuck, mainStartNewYear, mainAskContinue, mainErrors, main) where

import qualified Control.Monad as CM
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Gift_History
import Gift_Pair
import Hat
import Player
import Players
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
  let freshHat :: Hat = hatMakeHat (players state)
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

mainSelectNewGiver :: IO State -> IO State
mainSelectNewGiver ioState = do
  state <- ioState
  let giverToRemove :: Giver = DM.fromJust (maybeGiver state)
      replenishedGiveeHat :: Hat = hatReturnDiscards (discards state) (giveeHat state)
      diminishedGiverHat :: Hat = hatRemovePuck giverToRemove (giverHat state)
   in do
        newGivee <- mainDrawPuck replenishedGiveeHat
        newGiver <- mainDrawPuck diminishedGiverHat
        return
          state
            { rosterName = rosterName state,
              rosterYear = rosterYear state,
              players = players state,
              giftYear = giftYear state,
              giveeHat = replenishedGiveeHat,
              giverHat = diminishedGiverHat,
              maybeGivee = newGivee,
              maybeGiver = newGiver,
              discards = Set.empty,
              quit = quit state
            }

mainGiveeIsSuccess :: IO State -> IO State
mainGiveeIsSuccess ioState = do
  state <- ioState
  let currentGiver :: Giver = DM.fromJust (maybeGiver state)
      currentGivee :: Givee = DM.fromJust (maybeGivee state)
      updatedGiveePlayers :: Players = playersUpdateMyGivee currentGiver currentGivee (giftYear state)(players state)
   in do
        return
          state
            { rosterName = rosterName state,
              rosterYear = rosterYear state,
              players = playersUpdateMyGiver currentGivee currentGiver (giftYear state)updatedGiveePlayers,
              giftYear = giftYear state,
              giveeHat = hatRemovePuck currentGivee (giveeHat state),
              giverHat = giverHat state,
              maybeGivee = Nothing,
              maybeGiver = maybeGiver state,
              discards = discards state,
              quit = quit state
            }













mainErrors :: IO State -> IO [PlayerKey]
mainErrors ioState = do
  state <- ioState
  let playerKeys :: [PlayerKey] = Map.keys (players state)
      playerErrors :: [PlayerKey] = [playerKeyMe | playerKeyMe <- playerKeys, let myGiverKey = playersGetMyGiver playerKeyMe (players state) (giftYear state), let myGiveeKey = playersGetMyGivee playerKeyMe (players state) (giftYear state), (playerKeyMe == myGiverKey) || (playerKeyMe == myGiveeKey)]
   in return playerErrors

mainPrintResults :: IO State -> IO State
mainPrintResults ioState = do
  state <- ioState
  errorList <- mainErrors ioState
  putStrLn ("\n" ++ rosterName state ++ " - Year " ++ show (rosterYear state + giftYear state) ++ " Gifts:\n")
  let playerKeys :: [PlayerKey] = Map.keys (players state)
  mapM_
    putStrLn
    [ do
        let pName = playersGetPlayerName playerKey (players state)
        let giveeKey = playersGetMyGivee playerKey (players state) (giftYear state)
        let giveeName = playersGetPlayerName giveeKey (players state)
        let giverKey = playersGetMyGiver playerKey (players state) (giftYear state)
        if (playerKey == giveeKey) && (playerKey == giverKey)
          then pName ++ " is neither **buying** for nor **receiving** from anyone - **ERROR**"
          else
            if playerKey == giverKey
              then pName ++ " is **receiving** from no one - **ERROR**"
              else
                if playerKey == giveeKey
                  then pName ++ " is **buying** for no one - **ERROR**"
                  else pName ++ " is buying for " ++ giveeName
      | playerKey <- playerKeys
    ]
  CM.unless (null errorList) $ do
    putStrLn "\nThere is a logic error in this year's pairings."
    putStrLn "Do you see how it occurs?"
    putStrLn "If not... call me and I'll explain!\n"
  return state

mainAskContinue :: IO State -> IO State
mainAskContinue ioState = do
  state <- ioState
  putStr "\nContinue? ('q' to quit): "
  SIO.hFlush SIO.stdout
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
main =
  do
    --    state <- mainStartNewYear (mainAskContinue (mainPrintResults (return mainBeatlesState)))
    --errors <- mainErrors (mainPrintResults (mainStartNewYear (return mainBeatlesState)))
    state1 <- mainPrintResults (return mainBeatlesState)
    state2 <- mainStartNewYear (return state1)
    state3 <- mainPrintResults (return state2)
    state4 <- mainSelectNewGiver (return state3)
    --print errors
    print state4

{-# LANGUAGE ScopedTypeVariables #-}

module Helpers (helpersReadFileIntoJsonString, helpersRosterOrQuit, helpersDrawPuck, helpersStartNewYear, helpersSelectNewGiver, helpersGiveeIsSuccess, helpersGiveeIsFailure, helpersErrorListIsEmpty, helpersPrintResults, helpersPrintStringGivingRoster, helpersPromptLine, helpersPrintAndAsk) where

import qualified Control.Concurrent.STM as STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Gift_History
import Gift_Pair
import Hat
import Players
import Roster
import qualified System.Exit as SE
import System.IO
import System.Random

type TVarPlayers = STM.TVar Players

type TVarGiftYear = STM.TVar GiftYear

type TVarGiveeHat = STM.TVar Hat

type TVarGiverHat = STM.TVar Hat

type TVarMaybeGivee = STM.TVar (Maybe Givee)

type TVarMaybeGiver = STM.TVar (Maybe Giver)

type TVarDiscards = STM.TVar Discards

helpersReadFileIntoJsonString :: FilePath -> IO (Maybe JsonString)
helpersReadFileIntoJsonString f = do
  result <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
  case result of
    Right r -> do
      let s = BS.unpack r
      return (Just s)
    Left _ -> return Nothing

helpersRosterOrQuit :: FilePath -> TVarPlayers -> IO (RosterName, RosterYear)
helpersRosterOrQuit fp tVarPlayers = do
  rosterStringMaybe :: Maybe JsonString <- helpersReadFileIntoJsonString fp
  case rosterStringMaybe of
    Just rs -> do
      let rosterMaybe :: Maybe Roster = rosterJsonStringToRoster rs
      case rosterMaybe of
        Just r -> do
          STM.atomically $ STM.writeTVar tVarPlayers (players r)
          return (rosterName r, rosterYear r)
        Nothing -> SE.exitWith (SE.ExitFailure 22)
    Nothing -> SE.exitWith (SE.ExitFailure 11)

helpersDrawPuck :: Hat -> IO (Maybe PlayerSymbol)
helpersDrawPuck hat =
  if Set.null hat
    then return Nothing
    else do
      n <- randomRIO (0, Prelude.length hat - 1)
      return (Just (Set.elemAt n hat))

helpersStartNewYear :: TVarGiftYear -> TVarPlayers -> TVarGiverHat -> TVarGiveeHat -> TVarMaybeGiver -> TVarMaybeGivee -> TVarDiscards -> IO ()
helpersStartNewYear tVarGiftYear tVarPlayers tVarGiverHat tVarGiveeHat tVarMaybeGiver tVarMaybeGivee tVarDiscards = do
  plrs <- STM.readTVarIO tVarPlayers
  let nhgr = hatMakeHat plrs
  let nhge = hatMakeHat plrs
  mgr <- helpersDrawPuck nhgr
  mge <- helpersDrawPuck nhge
  STM.atomically $ do
    STM.modifyTVar tVarGiftYear (+ 1)
    STM.modifyTVar tVarPlayers playersAddYear
    STM.writeTVar tVarGiverHat nhgr
    STM.writeTVar tVarGiveeHat nhge
    STM.writeTVar tVarMaybeGiver mgr
    STM.writeTVar tVarMaybeGivee mge
    STM.writeTVar tVarDiscards Set.empty

helpersSelectNewGiver :: TVarMaybeGiver -> TVarGiverHat -> TVarGiveeHat -> TVarDiscards -> TVarMaybeGivee -> IO ()
helpersSelectNewGiver tVarMaybeGiver tVarGiverHat tVarGiveeHat tVarDiscards tVarMaybeGivee = do
  mgr <- STM.readTVarIO tVarMaybeGiver
  dc <- STM.readTVarIO tVarDiscards
  grh <- STM.readTVarIO tVarGiverHat
  mgr1 <- helpersDrawPuck grh
  geh <- STM.readTVarIO tVarGiveeHat
  mge <- helpersDrawPuck geh
  STM.atomically $ do
    STM.modifyTVar tVarGiverHat (hatRemovePuck (fromJust mgr))
    STM.modifyTVar tVarGiveeHat (hatReturnDiscards dc)
    STM.writeTVar tVarDiscards Set.empty
    STM.writeTVar tVarMaybeGiver mgr1
    STM.writeTVar tVarMaybeGivee mge

helpersGiveeIsSuccess :: TVarMaybeGiver -> TVarGiftYear -> TVarMaybeGivee -> TVarPlayers -> TVarGiveeHat -> IO ()
helpersGiveeIsSuccess tVarMaybeGiver tVarGiftYear tVarMaybeGivee tVarPlayers tVarGiveeHat = do
  mgr <- STM.readTVarIO tVarMaybeGiver
  gy <- STM.readTVarIO tVarGiftYear
  mge <- STM.readTVarIO tVarMaybeGivee
  STM.atomically $ do
    STM.modifyTVar tVarPlayers (playersUpdateGivee (fromJust mgr) (fromJust mge) gy)
    STM.modifyTVar tVarPlayers (playersUpdateGiver (fromJust mge) (fromJust mgr) gy)
    STM.modifyTVar tVarGiveeHat (hatRemovePuck (fromJust mge))
    STM.writeTVar tVarMaybeGivee Nothing

helpersGiveeIsFailure :: TVarMaybeGivee -> TVarGiveeHat -> TVarDiscards -> IO ()
helpersGiveeIsFailure tVarMaybeGivee tVarGiveeHat tVarDiscards = do
  mge <- STM.readTVarIO tVarMaybeGivee
  geh <- STM.readTVarIO tVarGiveeHat
  mge1 <- helpersDrawPuck geh
  STM.atomically $ do
    STM.modifyTVar tVarGiveeHat (hatRemovePuck (fromJust mge))
    STM.modifyTVar tVarDiscards (hatDiscardGivee (fromJust mge))
    STM.writeTVar tVarMaybeGivee mge1

helpersErrorListIsEmpty :: TVarPlayers -> TVarGiftYear -> IO Bool
helpersErrorListIsEmpty tVarPlayers tVarGiftYear = do
  plrs <- STM.readTVarIO tVarPlayers
  gy <- STM.readTVarIO tVarGiftYear
  let plrKeys = Map.keys plrs
  let errorList = [plrSymbol | plrSymbol <- plrKeys, let geeCode = playersGetGivee plrSymbol plrs gy, let gerCode = playersGetGiver plrSymbol plrs gy, (plrSymbol == gerCode) || (plrSymbol == geeCode)]
  return (null errorList)

helpersPrintResults :: TVarPlayers -> TVarGiftYear -> IO ()
helpersPrintResults tVarPlayers tVarGiftYear = do
  plrs <- STM.readTVarIO tVarPlayers
  gy <- STM.readTVarIO tVarGiftYear
  errorListIsEmpty <- helpersErrorListIsEmpty tVarPlayers tVarGiftYear
  let plrKeys = Map.keys plrs
  mapM_
    putStrLn
    [ do
        let pn = playersGetPlayerName plrSymbol plrs
        let geeCode = playersGetGivee plrSymbol plrs gy
        let geeName = playersGetPlayerName geeCode plrs
        let gerCode = playersGetGiver plrSymbol plrs gy
        if (plrSymbol == geeCode) && (plrSymbol == gerCode)
          then pn ++ " is neither **buying** for nor **receiving** from anyone - **ERROR**"
          else
            if plrSymbol == gerCode
              then pn ++ " is **receiving** from no one - **ERROR**"
              else
                if plrSymbol == geeCode
                  then pn ++ " is **buying** for no one - **ERROR**"
                  else pn ++ " is buying for " ++ geeName
      | plrSymbol <- plrKeys
    ]
  unless errorListIsEmpty $ do
    putStrLn "\nThere is a logic error in this year's pairings."
    putStrLn "Do you see how it occurs?"
    putStrLn "If not... call me and I'll explain!"

helpersPrintStringGivingRoster :: RosterName -> RosterYear -> TVarGiftYear -> TVarPlayers -> IO ()
helpersPrintStringGivingRoster rn ry tVarGiftYear tVarPlayers = do
  gy <- STM.readTVarIO tVarGiftYear
  putStrLn ("\n" ++ rn ++ " - Year " ++ show (ry + gy) ++ " Gifts:\n")
  helpersPrintResults tVarPlayers tVarGiftYear

helpersPromptLine :: String -> IO String
helpersPromptLine prompt = do
  putStr prompt
  hFlush stdout
  getLine

helpersPrintAndAsk :: RosterName -> RosterYear -> TVarGiftYear -> TVarPlayers -> IO String
helpersPrintAndAsk rn ry tVarGiftYear tVarPlayers = do
  helpersPrintStringGivingRoster rn ry tVarGiftYear tVarPlayers
  helpersPromptLine "\nContinue? ('q' to quit): "

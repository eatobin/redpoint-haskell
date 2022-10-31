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
import System.Exit
import System.IO
import System.Random

type ErrorString = String

type TVarPlayers = STM.TVar Players

type TVarGiftYear = STM.TVar GiftYear

type TVarGiveeHat = STM.TVar Hat

type TVarGiverHat = STM.TVar Hat

type TVarMaybeGivee = STM.TVar (Maybe Givee)

type TVarMaybeGiver = STM.TVar (Maybe Giver)

type TVarDiscards = STM.TVar Discards

helpersReadFileIntoJsonString :: FilePath -> IO (Either ErrorString JsonString)
helpersReadFileIntoJsonString f = do
  result <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
  case result of
    Right r -> do
      let s = BS.unpack r
      return (Right s)
    Left _ -> return (Left "file read error.")

helpersRosterOrQuit :: FilePath -> TVarPlayers -> IO (RosterName, RosterYear)
helpersRosterOrQuit fp tvPlayers = do
  rosterStringEither :: Either ErrorString JsonString <- helpersReadFileIntoJsonString fp
  case rosterStringEither of
    Right rs -> do
      let maybeRoster :: Maybe Roster = rosterJsonStringToRoster rs
      case maybeRoster of
        Just r -> do
          STM.atomically $ STM.writeTVar tvPlayers (players r)
          return (rosterName r, rosterYear r)
        Nothing -> exitWith (ExitFailure 42)
    Left _ -> exitWith (ExitFailure 99)

helpersDrawPuck :: Hat -> IO (Maybe PlayerSymbol)
helpersDrawPuck hat =
  if Set.null hat
    then return Nothing
    else do
      n <- randomRIO (0, Prelude.length hat - 1)
      return (Just (Set.elemAt n hat))

helpersStartNewYear :: TVarGiftYear -> TVarPlayers -> TVarGiverHat -> TVarGiveeHat -> TVarMaybeGiver -> TVarMaybeGivee -> TVarDiscards -> IO ()
helpersStartNewYear tvGiftYear tvPlayers tvGiverHat tvGiveeHat tvMaybeGiver tvMaybeGivee tvDiscards = do
  plrs <- STM.readTVarIO tvPlayers
  let nhgr = hatMakeHat plrs
  let nhge = hatMakeHat plrs
  mgr <- helpersDrawPuck nhgr
  mge <- helpersDrawPuck nhge
  STM.atomically $ STM.modifyTVar tvGiftYear (+ 1)
  STM.atomically $ STM.modifyTVar tvPlayers playersAddYear
  STM.atomically $ STM.writeTVar tvGiverHat nhgr
  STM.atomically $ STM.writeTVar tvGiveeHat nhge
  STM.atomically $ STM.writeTVar tvMaybeGiver mgr
  STM.atomically $ STM.writeTVar tvMaybeGivee mge
  STM.atomically $ STM.writeTVar tvDiscards Set.empty

helpersSelectNewGiver :: TVarMaybeGiver -> TVarGiverHat -> TVarGiveeHat -> TVarDiscards -> TVarMaybeGivee -> IO ()
helpersSelectNewGiver tvMaybeGiver tvGiverHat tvGiveeHat tvDiscards tvMaybeGivee = do
  mgr <- STM.readTVarIO tvMaybeGiver
  STM.atomically $ STM.modifyTVar tvGiverHat (hatRemovePuck (fromJust mgr))
  dc <- STM.readTVarIO tvDiscards
  STM.atomically $ STM.modifyTVar tvGiveeHat (hatReturnDiscards dc)
  STM.atomically $ STM.writeTVar tvDiscards Set.empty
  grh <- STM.readTVarIO tvGiverHat
  mgr1 <- helpersDrawPuck grh
  STM.atomically $ STM.writeTVar tvMaybeGiver mgr1
  geh <- STM.readTVarIO tvGiveeHat
  mge <- helpersDrawPuck geh
  STM.atomically $ STM.writeTVar tvMaybeGivee mge

helpersGiveeIsSuccess :: TVarMaybeGiver -> TVarGiftYear -> TVarMaybeGivee -> TVarPlayers -> TVarGiveeHat -> IO ()
helpersGiveeIsSuccess tvMaybeGiver tvGiftYear tvMaybeGivee tvPlayers tvGiveeHat = do
  mgr <- STM.readTVarIO tvMaybeGiver
  gy <- STM.readTVarIO tvGiftYear
  mge <- STM.readTVarIO tvMaybeGivee
  STM.atomically $ STM.modifyTVar tvPlayers (playersUpdateGivee (fromJust mgr) (fromJust mge) gy)
  STM.atomically $ STM.modifyTVar tvPlayers (playersUpdateGiver (fromJust mge) (fromJust mgr) gy)
  STM.atomically $ STM.modifyTVar tvGiveeHat (hatRemovePuck (fromJust mge))
  STM.atomically $ STM.writeTVar tvMaybeGivee Nothing

helpersGiveeIsFailure :: TVarMaybeGivee -> TVarGiveeHat -> TVarDiscards -> IO ()
helpersGiveeIsFailure tvMaybeGivee tvGiveeHat tvDiscards = do
  mge <- STM.readTVarIO tvMaybeGivee
  STM.atomically $ STM.modifyTVar tvGiveeHat (hatRemovePuck (fromJust mge))
  STM.atomically $ STM.modifyTVar tvDiscards (hatDiscardGivee (fromJust mge))
  geh <- STM.readTVarIO tvGiveeHat
  mge1 <- helpersDrawPuck geh
  STM.atomically $ STM.writeTVar tvMaybeGivee mge1

helpersErrorListIsEmpty :: TVarPlayers -> TVarGiftYear -> IO Bool
helpersErrorListIsEmpty tvPlayers tvGiftYear = do
  plrs <- STM.readTVarIO tvPlayers
  gy <- STM.readTVarIO tvGiftYear
  let plrKeys = Map.keys plrs
  let errorList = [plrSymbol | plrSymbol <- plrKeys, let geeCode = playersGetGivee plrSymbol plrs gy, let gerCode = playersGetGiver plrSymbol plrs gy, (plrSymbol == gerCode) || (plrSymbol == geeCode)]
  return (null errorList)

helpersPrintResults :: TVarPlayers -> TVarGiftYear -> IO ()
helpersPrintResults tvPlayers tvGiftYear = do
  plrs <- STM.readTVarIO tvPlayers
  gy <- STM.readTVarIO tvGiftYear
  errorListIsEmpty <- helpersErrorListIsEmpty tvPlayers tvGiftYear
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
helpersPrintStringGivingRoster rn ry tvGiftYear tvPlayers = do
  gy <- STM.readTVarIO tvGiftYear
  putStrLn ("\n" ++ rn ++ " - Year " ++ show (ry + gy) ++ " Gifts:\n")
  helpersPrintResults tvPlayers tvGiftYear

helpersPromptLine :: String -> IO String
helpersPromptLine prompt = do
  putStr prompt
  hFlush stdout
  getLine

helpersPrintAndAsk :: RosterName -> RosterYear -> TVarGiftYear -> TVarPlayers -> IO String
helpersPrintAndAsk rn ry tvGiftYear tvPlayers = do
  helpersPrintStringGivingRoster rn ry tvGiftYear tvPlayers
  helpersPromptLine "\nContinue? ('q' to quit): "

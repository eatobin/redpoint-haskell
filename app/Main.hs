{-# LANGUAGE ScopedTypeVariables #-}

module Main (main, mainReadFileIntoJsonString, mainRosterOrQuit, mainDrawPuck, mainStartNewYear) where

import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Gift_History
import Gift_Pair
import Hat
import Players
import Roster
import System.Random

type ErrorString = String

type TVRosterName = TVar RosterName

type TVRosterYear = TVar RosterYear

type TVPlayers = TVar Players

type TVGiftYear = TVar GiftYear

type TVGiveeHat = TVar Hat

type TVGiverHat = TVar Hat

type TVMaybeGivee = TVar (Maybe Givee)

type TVMaybeGiver = TVar (Maybe Giver)

type TVDiscards = TVar Discards

filePath :: FilePath
filePath = "resources/blackhawks.json"

main :: IO ()
main = do
  tvGiftYear <- atomically (newTVar 0)
  tvMaybeGiver <- atomically (newTVar Nothing)
  tvMaybeGivee <- atomically (newTVar Nothing)
  tvPlayers <- atomically (newTVar Map.empty)
  tvGiverHat <- atomically (newTVar Set.empty)
  tvGiveeHat <- atomically (newTVar Set.empty)
  tvDiscards <- atomically (newTVar Set.empty)
  tvRosterName <- atomically (newTVar "")
  tvRosterYear <- atomically (newTVar 0)
  mainRosterOrQuit filePath tvRosterName tvRosterYear tvPlayers
  mainStartNewYear tvGiftYear tvPlayers tvGiverHat tvGiveeHat tvMaybeGiver tvMaybeGivee tvDiscards
  gy <- readTVarIO tvGiftYear
  mgr <- readTVarIO tvMaybeGiver
  mge <- readTVarIO tvMaybeGivee
  plrs <- readTVarIO tvPlayers
  grh <- readTVarIO tvGiverHat
  geh <- readTVarIO tvGiveeHat
  dis <- readTVarIO tvDiscards
  rn <- readTVarIO tvRosterName
  ry <- readTVarIO tvRosterYear
  print gy
  print mgr
  print mge
  print plrs
  print grh
  print geh
  print dis
  print rn
  print ry

mainReadFileIntoJsonString :: FilePath -> IO (Either ErrorString JsonString)
mainReadFileIntoJsonString f = do
  result <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
  case result of
    Right r -> do
      let s = BS.unpack r
      return (Right s)
    Left _ -> return (Left "File read error.")

mainRosterOrQuit :: FilePath -> TVRosterName -> TVRosterYear -> TVPlayers -> IO ()
mainRosterOrQuit fp tvRosterName tvRosterYear tvPlayers = do
  rosterStringEither :: Either ErrorString JsonString <- mainReadFileIntoJsonString fp
  case rosterStringEither of
    Right rs -> do
      let maybeRoster :: Maybe Roster = rosterJsonStringToRoster rs
      case maybeRoster of
        Just r -> do
          atomically $ writeTVar tvRosterName (rosterName r)
          atomically $ writeTVar tvRosterYear (rosterYear r)
          atomically $ writeTVar tvPlayers (players r)
        Nothing -> putStrLn "parse error"
    Left fe -> putStrLn fe

mainDrawPuck :: Hat -> IO (Maybe PlayerSymbol)
mainDrawPuck hat =
  if Set.null hat
    then return Nothing
    else do
      n <- randomRIO (0, length hat - 1)
      return (Just (Set.elemAt n hat))

mainStartNewYear :: TVGiftYear -> TVPlayers -> TVGiverHat -> TVGiveeHat -> TVMaybeGiver -> TVMaybeGivee -> TVDiscards -> IO ()
mainStartNewYear tvGiftYear tvPlayers tvGiverHat tvGiveeHat tvMaybeGiver tvMaybeGivee tvDiscards = do
  plrs <- readTVarIO tvPlayers
  let nhgr = hatMakeHat plrs
  let nhge = hatMakeHat plrs
  gr <- mainDrawPuck nhgr
  ge <- mainDrawPuck nhge
  atomically $ modifyTVar tvGiftYear (+ 1)
  atomically $ modifyTVar tvPlayers playersAddYear
  atomically $ writeTVar tvGiverHat nhgr
  atomically $ writeTVar tvGiveeHat nhge
  atomically $ writeTVar tvMaybeGiver gr
  atomically $ writeTVar tvMaybeGivee ge
  atomically $ writeTVar tvDiscards Set.empty

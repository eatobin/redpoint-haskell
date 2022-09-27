{-# LANGUAGE ScopedTypeVariables #-}

module Main (main, redpointRosterOrQuit) where

import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Gift_Pair
import Player
import Players
import Roster

type ErrorString = String

type TVRosterName = TVar RosterName

type TVRosterYear = TVar RosterYear

type TVPlayers = TVar Players

jsonFile :: FilePath
jsonFile = "resources/blackhawks.json"

main :: IO ()
main = do
  tvRosterName <- atomically (newTVar "nope")
  tvRosterYear <- atomically (newTVar 0)
  tvPlayers <- atomically (newTVar (Map.fromList [("GeoHar", Player {playerName = "George Harrison", giftHistory = Seq.fromList [GiftPair {givee = "RinSta", giver = "PauMcc"}]})]))
  redpointRosterOrQuit jsonFile tvRosterName tvRosterYear tvPlayers
  rn <- readTVarIO tvRosterName
  ry <- readTVarIO tvRosterYear
  plrs <- readTVarIO tvPlayers
  putStrLn rn
  print ry
  print plrs

mainReadFileIntoJsonString :: FilePath -> IO (Either ErrorString JsonString)
mainReadFileIntoJsonString f = do
  result <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
  case result of
    Right r -> do
      let s = BS.unpack r
      return (Right s)
    Left _ -> return (Left "File read error.")

redpointRosterOrQuit :: FilePath -> TVRosterName -> TVRosterYear -> TVPlayers -> IO ()
redpointRosterOrQuit filePath tvRosterName tvRosterYear tvPlayers = do
  rosterStringEither :: Either ErrorString JsonString <- mainReadFileIntoJsonString filePath
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

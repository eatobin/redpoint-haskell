{-# LANGUAGE ScopedTypeVariables #-}

module Main (main, mainReadFileIntoJsonString, mainRosterOrQuit, mainDrawPuck, mainStartNewYear, mainSelectNewGiver, mainGiveeIsSuccess, mainGiveeIsFailure, mainErrors, mainPrintResults, testingPrintResults) where

import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence as Seq
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

type PlayerErrors = Seq.Seq PlayerSymbol

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
      n <- randomRIO (0, Prelude.length hat - 1)
      return (Just (Set.elemAt n hat))

mainStartNewYear :: TVGiftYear -> TVPlayers -> TVGiverHat -> TVGiveeHat -> TVMaybeGiver -> TVMaybeGivee -> TVDiscards -> IO ()
mainStartNewYear tvGiftYear tvPlayers tvGiverHat tvGiveeHat tvMaybeGiver tvMaybeGivee tvDiscards = do
  plrs <- readTVarIO tvPlayers
  let nhgr = hatMakeHat plrs
  let nhge = hatMakeHat plrs
  mgr <- mainDrawPuck nhgr
  mge <- mainDrawPuck nhge
  atomically $ modifyTVar tvGiftYear (+ 1)
  atomically $ modifyTVar tvPlayers playersAddYear
  atomically $ writeTVar tvGiverHat nhgr
  atomically $ writeTVar tvGiveeHat nhge
  atomically $ writeTVar tvMaybeGiver mgr
  atomically $ writeTVar tvMaybeGivee mge
  atomically $ writeTVar tvDiscards Set.empty

mainSelectNewGiver :: TVMaybeGiver -> TVGiverHat -> TVGiveeHat -> TVDiscards -> TVMaybeGivee -> IO ()
mainSelectNewGiver tvMaybeGiver tvGiverHat tvGiveeHat tvDiscards tvMaybeGivee = do
  mgr <- readTVarIO tvMaybeGiver
  atomically $ modifyTVar tvGiverHat (hatRemovePuck (fromJust mgr))
  dc <- readTVarIO tvDiscards
  atomically $ modifyTVar tvGiveeHat (hatReturnDiscards dc)
  atomically $ writeTVar tvDiscards Set.empty
  grh <- readTVarIO tvGiverHat
  mgr1 <- mainDrawPuck grh
  atomically $ writeTVar tvMaybeGiver mgr1
  geh <- readTVarIO tvGiveeHat
  mge <- mainDrawPuck geh
  atomically $ writeTVar tvMaybeGivee mge

mainGiveeIsSuccess :: TVMaybeGiver -> TVGiftYear -> TVMaybeGivee -> TVPlayers -> TVGiveeHat -> IO ()
mainGiveeIsSuccess tvMaybeGiver tvGiftYear tvMaybeGivee tvPlayers tvGiveeHat = do
  mgr <- readTVarIO tvMaybeGiver
  gy <- readTVarIO tvGiftYear
  mge <- readTVarIO tvMaybeGivee
  atomically $ modifyTVar tvPlayers (playersUpdateGivee (fromJust mgr) (fromJust mge) gy)
  atomically $ modifyTVar tvPlayers (playersUpdateGiver (fromJust mge) (fromJust mgr) gy)
  atomically $ modifyTVar tvGiveeHat (hatRemovePuck (fromJust mge))
  atomically $ writeTVar tvMaybeGivee Nothing

mainGiveeIsFailure :: TVMaybeGivee -> TVGiveeHat -> TVDiscards -> IO ()
mainGiveeIsFailure tvMaybeGivee tvGiveeHat tvDiscards = do
  mge <- readTVarIO tvMaybeGivee
  atomically $ modifyTVar tvGiveeHat (hatRemovePuck (fromJust mge))
  atomically $ modifyTVar tvDiscards (hatDiscardGivee (fromJust mge))
  geh <- readTVarIO tvGiveeHat
  mge1 <- mainDrawPuck geh
  atomically $ writeTVar tvMaybeGivee mge1

mainErrors :: TVPlayers -> GiftYear -> IO PlayerErrors
mainErrors tvPlayers tvGiftYear = do
  plrs <- readTVarIO tvPlayers
  let plrKeys = Map.keys plrs
  return
    ( Seq.fromList
        [ plrSymbol
          | plrSymbol <- plrKeys,
            let geeCode = playersGetGivee plrSymbol plrs tvGiftYear,
            let gerCode = playersGetGiver plrSymbol plrs tvGiftYear,
            (plrSymbol == gerCode) || (plrSymbol == geeCode)
        ]
    )

mainPrintResults :: TVPlayers -> GiftYear -> IO ()
mainPrintResults tvPlayers tvGiftYear = do
  plrs <- readTVarIO tvPlayers
  let plrKeys = Map.keys plrs
  mapM_
    putStrLn
    [ pn ++ " is buying for " ++ geeName
      | plrSymbol <- plrKeys,
        let pn = playersGetPlayerName plrSymbol plrs,
        let geeCode = playersGetGivee plrSymbol plrs tvGiftYear,
        let geeName = playersGetPlayerName geeCode plrs
        --        let gerCode = playersGetGiver plrSymbol plrs tvGiftYear
    ]

--testingPrintResults :: Players -> GiftYear -> IO ()
--testingPrintResults plrs gy = do
--  let plrKeys = Map.keys plrs
--  mapM_
--    putStrLn
--    [ pn ++ " is buying for " ++ geeName
--      | plrSymbol <- plrKeys,
--        let pn = playersGetPlayerName plrSymbol plrs,
--        let geeCode = playersGetGivee plrSymbol plrs gy,
--        let geeName = playersGetPlayerName geeCode plrs,
--        let gerCode = playersGetGiver plrSymbol plrs gy,
--        if (plrSymbol == gerCode) || (plrSymbol == geeCode)
--          then pn ++ " is buying for " ++ geeName
--        else pn ++ " is buying for " ++ geeName
--    ]

--testingPrintResults :: Players -> IO ()

--testingPrintResults :: Map.Map String a -> IO ()
--testingPrintResults plrs = do
--  let plrKeys = Map.keys plrs
--  mapM_ putStrLn [plrSymbol | plrSymbol <- plrKeys]

testingPrintResults :: Players -> GiftYear -> IO ()
testingPrintResults plrs gy = do
  let plrKeys = Map.keys plrs
  mapM_
    putStrLn
    [ do
        let pn = playersGetPlayerName plrSymbol plrs
        let geeCode = playersGetGivee plrSymbol plrs gy
        let geeName = playersGetPlayerName geeCode plrs
        let gerCode = playersGetGiver plrSymbol plrs gy
        if (plrSymbol == gerCode) || True
          then pn ++ " " ++ geeCode ++ " " ++ geeName ++ " " ++ gerCode
          else "Nope!!"
      | plrSymbol <- plrKeys
    ]

--  def redpointPrintResults(): Unit = {
--    val plrKeys: Seq[String] = aPlayers.keys.toSeq.sorted
--    for (plrSym <- plrKeys) yield {
--      val playerName = playersGetPlayerName(plrSym, aPlayers)
--      val giveeCode = playersGetGivee(plrSym, agYear, aPlayers)
--      val giveeName = playersGetPlayerName(giveeCode, aPlayers)
--      val giverCode = playersGetGiver(plrSym, agYear, aPlayers)
--
--      if (plrSymbol == giveeCode && plrSym == giverCode) {
--        println("%s is **buying** for nor **receiving** from anyone - **ERROR**".format(playerName))
--      } else if (plrSym == giverCode) {
--        println("%s is **receiving** from no one - **ERROR**".format(playerName))
--      } else if (plrSym == giveeCode) {
--        println("%s is **buying** for no one - **ERROR**".format(playerName))
--      } else {
--        println("%s is buying for %s".format(playerName, giveeName))
--      }
--    }
--    if (redpointErrors().nonEmpty) {
--      println()
--      println("There is a logic error in this year's pairings.")
--      println("Do you see how it occurs?")
--      println("If not... call me and I'll explain!")
--    }
--  }

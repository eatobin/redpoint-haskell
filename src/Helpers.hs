{-# LANGUAGE ScopedTypeVariables #-}

module Helpers (helpersReadFileIntoJsonString, helpersRosterOrQuit, helpersDrawPuck, helpersStartNewYear, helpersSelectNewGiver, helpersGiveeIsSuccess, helpersGiveeIsFailure, helpersErrorListIsEmpty, helpersPrintResults, helpersPrintStringGivingRoster, helpersPromptLine, helpersPrintAndAsk) where

import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as CE
import qualified Control.Monad as CM
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM
import qualified Data.Set as Set
import Gift_History
import Gift_Pair
import Hat
import Players
import Roster
import System.IO
import System.Random

type ErrorString = String

type TVarRosterName = STM.TVar RosterName

type TVarRosterYear = STM.TVar RosterYear

type TVarPlayers = STM.TVar Players

type TVarGiftYear = STM.TVar GiftYear

type TVarGiveeHat = STM.TVar Hat

type TVarGiverHat = STM.TVar Hat

type TVarMaybeGivee = STM.TVar (Maybe Givee)

type TVarMaybeGiver = STM.TVar (Maybe Giver)

type TVarDiscards = STM.TVar Discards

helpersReadFileIntoJsonString :: FilePath -> IO (Either ErrorString JsonString)
helpersReadFileIntoJsonString f = do
  result <- CE.try (BS.readFile f) :: IO (Either CE.SomeException BS.ByteString)
  case result of
    Right r -> do
      let s = BS.unpack r
      return (Right s)
    Left _ -> return (Left "file read error")

helpersRosterOrQuit :: FilePath -> TVarRosterName -> TVarRosterYear -> TVarPlayers -> IO ()
helpersRosterOrQuit fp tVarRosterName tVarRosterYear tVarPlayers = do
  rosterStringEither :: Either ErrorString JsonString <- helpersReadFileIntoJsonString fp
  case rosterStringEither of
    Right rs -> do
      let maybeRoster :: Maybe Roster = rosterJsonStringToRoster rs
      case maybeRoster of
        Just r -> STM.atomically $ do
          STM.writeTVar tVarRosterName (rosterName r)
          STM.writeTVar tVarRosterYear (rosterYear r)
          STM.writeTVar tVarPlayers (players r)
        Nothing -> putStrLn "roster parse error"
    Left fe -> putStrLn fe

helpersDrawPuck :: Hat -> IO (Maybe PlayerKey)
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
    STM.modifyTVar tVarGiverHat (hatRemovePuck (DM.fromJust mgr))
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
    STM.modifyTVar tVarPlayers (playersUpdateMyGivee (DM.fromJust mgr) (DM.fromJust mge) gy)
    STM.modifyTVar tVarPlayers (playersUpdateMyGiver (DM.fromJust mge) (DM.fromJust mgr) gy)
    STM.modifyTVar tVarGiveeHat (hatRemovePuck (DM.fromJust mge))
    STM.writeTVar tVarMaybeGivee Nothing

helpersGiveeIsFailure :: TVarMaybeGivee -> TVarGiveeHat -> TVarDiscards -> IO ()
helpersGiveeIsFailure tVarMaybeGivee tVarGiveeHat tVarDiscards = do
  mge <- STM.readTVarIO tVarMaybeGivee
  geh <- STM.readTVarIO tVarGiveeHat
  mge1 <- helpersDrawPuck geh
  STM.atomically $ do
    STM.modifyTVar tVarGiveeHat (hatRemovePuck (DM.fromJust mge))
    STM.modifyTVar tVarDiscards (hatDiscardGivee (DM.fromJust mge))
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
        let gerCode = playersGetMyGiver plrSymbol plrs gy
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
  CM.unless errorListIsEmpty $ do
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

helpersPrintAndAsk :: TVarRosterName -> TVarRosterYear -> TVarGiftYear -> TVarPlayers -> IO String
helpersPrintAndAsk tVarRosterName tVarRosterYear tVarGiftYear tVarPlayers = do
  rn <- STM.readTVarIO tVarRosterName
  ry <- STM.readTVarIO tVarRosterYear
  helpersPrintStringGivingRoster rn ry tVarGiftYear tVarPlayers
  helpersPromptLine "\nContinue? ('q' to quit): "

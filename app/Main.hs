{-# LANGUAGE ScopedTypeVariables #-}

module Main (main, mainHelpersReadFileIntoJsonString, mainRosterOrQuit, mainDrawPuck, mainStartNewYear, mainSelectNewGiver, mainGiveeIsSuccess, mainGiveeIsFailure, mainErrorListIsEmpty, mainPrintResults, mainPrintStringGivingRoster, mainPromptLine, mainPrintAndAsk) where

import Control.Concurrent.STM
--import Control.Exception
import Control.Monad
import Control.Monad.Loops (whileM_)
--import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Gift_History
import Gift_Pair
import Hat
import Players
import Roster
import Rules
import System.Exit
import System.IO
import System.Random
import Main_Helpers

type ErrorString = String

--type TVRosterName = TVar RosterName

--type TVRosterYear = TVar RosterYear

type TVPlayers = TVar Players

type TVGiftYear = TVar GiftYear

type TVGiveeHat = TVar Hat

type TVGiverHat = TVar Hat

type TVMaybeGivee = TVar (Maybe Givee)

type TVMaybeGiver = TVar (Maybe Giver)

type TVDiscards = TVar Discards

--filePath :: FilePath
--filePath = "resources/blackhawks.json"

main :: IO ()
main = do
  tvGiftYear <- atomically (newTVar 0)
  tvMaybeGiver <- atomically (newTVar Nothing)
  tvMaybeGivee <- atomically (newTVar Nothing)
  tvPlayers <- atomically (newTVar Map.empty)
  tvGiverHat <- atomically (newTVar Set.empty)
  tvGiveeHat <- atomically (newTVar Set.empty)
  tvDiscards <- atomically (newTVar Set.empty)
  rosterPair <- mainRosterOrQuit filePath tvPlayers
  let rn = fst rosterPair
  let ry = snd rosterPair
  --  mainStartNewYear tvGiftYear tvPlayers tvGiverHat tvGiveeHat tvMaybeGiver tvMaybeGivee tvDiscards
  --  gyX <- readTVarIO tvGiftYear
  --  mgrX <- readTVarIO tvMaybeGiver
  --  mgeX <- readTVarIO tvMaybeGivee
  --  plrsX <- readTVarIO tvPlayers
  --  grh <- readTVarIO tvGiverHat
  --  geh <- readTVarIO tvGiveeHat
  --  dis <- readTVarIO tvDiscards
  --  rn <- readTVarIO tvRosterName
  --  ry <- readTVarIO tvRosterYear
  --  print gyX
  --  print mgrX
  --  print mgeX
  --  print plrsX
  --  print grh
  --  print geh
  --  print dis
  --  print rn
  --  print ry
  --  mainPrintStringGivingRoster rn ry tvGiftYear tvPlayers
  --  a <- mainPrintAndAsk rn ry tvGiftYear tvPlayers
  --  putStrLn a

  --  mainRosterOrQuit filePath tvRosterName tvRosterYear tvPlayers
  --  rn <- readTVarIO tvRosterName
  --  ry <- readTVarIO tvRosterYear
  whileM_ ((/= "q") . map toLower <$> mainPrintAndAsk rn ry tvGiftYear tvPlayers) $ do
    mainStartNewYear tvGiftYear tvPlayers tvGiverHat tvGiveeHat tvMaybeGiver tvMaybeGivee tvDiscards
    whileM_ (fmap isJust (readTVarIO tvMaybeGiver)) $ do
      whileM_ (fmap isJust (readTVarIO tvMaybeGivee)) $ do
        mgr <- readTVarIO tvMaybeGiver
        mge <- readTVarIO tvMaybeGivee
        gy <- readTVarIO tvGiftYear
        plrs <- readTVarIO tvPlayers
        if rulesGiveeNotSelf (fromJust mgr) (fromJust mge)
          && rulesGiveeNotReciprocal (fromJust mgr) plrs gy (fromJust mge)
          && rulesGiveeNotRepeat (fromJust mgr) (fromJust mge) gy plrs
          then mainGiveeIsSuccess tvMaybeGiver tvGiftYear tvMaybeGivee tvPlayers tvGiveeHat
          else mainGiveeIsFailure tvMaybeGivee tvGiveeHat tvDiscards
      mainSelectNewGiver tvMaybeGiver tvGiverHat tvDiscards tvGiveeHat tvMaybeGivee
    putStrLn ""
  putStrLn ""
  putStrLn "This was fun!"
  putStrLn "Talk about a position with Redpoint?"
  putStrLn "Please call: Eric Tobin 773-325-1516"
  putStrLn "Thanks! Bye..."
  putStrLn ""

--mainReadFileIntoJsonString :: FilePath -> IO (Either ErrorString JsonString)
--mainReadFileIntoJsonString f = do
--  result <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
--  case result of
--    Right r -> do
--      let s = BS.unpack r
--      return (Right s)
--    Left _ -> return (Left "File read error.")

mainRosterOrQuit :: FilePath -> TVPlayers -> IO (RosterName, RosterYear)
mainRosterOrQuit fp tvPlayers = do
  rosterStringEither :: Either ErrorString JsonString <- mainHelpersReadFileIntoJsonString fp
  case rosterStringEither of
    Right rs -> do
      let maybeRoster :: Maybe Roster = rosterJsonStringToRoster rs
      case maybeRoster of
        Just r -> do
          atomically $ writeTVar tvPlayers (players r)
          return (rosterName r, rosterYear r)
        Nothing -> exitWith (ExitFailure 42)
    Left _ -> exitWith (ExitFailure 99)

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

mainErrorListIsEmpty :: TVPlayers -> TVGiftYear -> IO Bool
mainErrorListIsEmpty tvPlayers tvGiftYear = do
  plrs <- readTVarIO tvPlayers
  gy <- readTVarIO tvGiftYear
  let plrKeys = Map.keys plrs
  let errorList = [plrSymbol | plrSymbol <- plrKeys, let geeCode = playersGetGivee plrSymbol plrs gy, let gerCode = playersGetGiver plrSymbol plrs gy, (plrSymbol == gerCode) || (plrSymbol == geeCode)]
  return (null errorList)

mainPrintResults :: TVPlayers -> TVGiftYear -> IO ()
mainPrintResults tvPlayers tvGiftYear = do
  plrs <- readTVarIO tvPlayers
  gy <- readTVarIO tvGiftYear
  errorListIsEmpty <- mainErrorListIsEmpty tvPlayers tvGiftYear
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

mainPrintStringGivingRoster :: RosterName -> RosterYear -> TVGiftYear -> TVPlayers -> IO ()
mainPrintStringGivingRoster rn ry tvGiftYear tvPlayers = do
  gy <- readTVarIO tvGiftYear
  putStrLn ("\n" ++ rn ++ " - Year " ++ show (ry + gy) ++ " Gifts:\n")
  mainPrintResults tvPlayers tvGiftYear

mainPromptLine :: String -> IO String
mainPromptLine prompt = do
  putStr prompt
  hFlush stdout
  getLine

mainPrintAndAsk :: RosterName -> RosterYear -> TVGiftYear -> TVPlayers -> IO String
mainPrintAndAsk rn ry tvGiftYear tvPlayers = do
  mainPrintStringGivingRoster rn ry tvGiftYear tvPlayers
  mainPromptLine "\nContinue? ('q' to quit): "

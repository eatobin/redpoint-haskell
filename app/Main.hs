{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Control.Concurrent.STM as STM
import Control.Exception
import Control.Monad
import Control.Monad.Loops (whileM_)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM
import qualified Data.Set as Set
import Gift_History
import Gift_Pair
import Hat
import Helpers
import Players
import Roster
import Rules
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

filePath :: FilePath
filePath = "resources-test/beatles.json"

main :: IO ()
main = do
  tVarPlayers <- STM.atomically (STM.newTVar (Map.empty :: Players))
  ioPair <- helpersRosterOrQuit filePath tVarPlayers
  tVarGiftYear <- STM.atomically (STM.newTVar (0 :: Int))
  tVarGiverHat <- STM.atomically (STM.newTVar Set.empty)
  tVarGiveeHat <- STM.atomically (STM.newTVar Set.empty)
  tVarMaybeGiver <- STM.atomically (STM.newTVar Nothing)
  tVarMaybeGivee <- STM.atomically (STM.newTVar Nothing)
  tVarDiscards <- STM.atomically (STM.newTVar Set.empty)
  let rn = fst ioPair
  let ry = snd ioPair
  plrsX <- STM.readTVarIO tVarPlayers
  gyX <- STM.readTVarIO tVarGiftYear
  mgrX <- STM.readTVarIO tVarMaybeGiver
  mgeX <- STM.readTVarIO tVarMaybeGivee
  grh :: Hat <- STM.readTVarIO tVarGiverHat
  geh :: Hat <- STM.readTVarIO tVarGiveeHat
  dis :: Discards <- STM.readTVarIO tVarDiscards

  print rn
  print ry
  print plrsX
  print gyX
  print (DM.fromMaybe "Nothing" mgrX)
  print (DM.fromMaybe "Nothing" mgeX)
  print grh
  print geh
  print dis

  helpersPrintStringGivingRoster rn ry tVarGiftYear tVarPlayers

  a <- helpersPrintAndAsk rn ry tVarGiftYear tVarPlayers
  putStrLn a

-- helpersStartNewYear tVarGiftYear tVarPlayers tVarGiverHat tVarGiveeHat tVarMaybeGiver tVarMaybeGivee tVarDiscards
--  mainRosterOrQuit filePath tvRosterName tvRosterYear TVarPlayers
--  rn <- STM.readTVarIO tvRosterName
--  ry <- STM.readTVarIO tvRosterYear
--  whileM_ ((/= "q") . map toLower <$> mainPrintAndAsk rn ry TVarGiftYear TVarPlayers) $ do
--    mainStartNewYear TVarGiftYear TVarPlayers TVarGiverHat TVarGiveeHat TVarMaybeGiver TVarMaybeGivee TVarDiscards
--    whileM_ (fmap isJust (STM.readTVarIO TVarMaybeGiver)) $ do
--      whileM_ (fmap isJust (STM.readTVarIO TVarMaybeGivee)) $ do
--        mgr <- STM.readTVarIO TVarMaybeGiver
--        mge <- STM.readTVarIO TVarMaybeGivee
--        gy <- STM.readTVarIO TVarGiftYear
--        plrs <- STM.readTVarIO TVarPlayers
--        if rulesGiveeNotSelf (fromJust mgr) (fromJust mge)
--          && rulesGiveeNotReciprocal (fromJust mgr) plrs gy (fromJust mge)
--          && rulesGiveeNotRepeat (fromJust mgr) (fromJust mge) gy plrs
--          then mainGiveeIsSuccess TVarMaybeGiver TVarGiftYear TVarMaybeGivee TVarPlayers TVarGiveeHat
--          else mainGiveeIsFailure TVarMaybeGivee TVarGiveeHat TVarDiscards
--      mainSelectNewGiver TVarMaybeGiver TVarGiverHat TVarDiscards TVarGiveeHat TVarMaybeGivee
--    putStrLn ""
--  putStrLn ""
--  putStrLn "This was fun!"
--  putStrLn "Talk about a position with Redpoint?"
--  putStrLn "Please call: Eric Tobin 773-325-1516"
--  putStrLn "Thanks! Bye..."
--  putStrLn ""

--mainReadFileIntoJsonString :: FilePath -> IO (Either ErrorString JsonString)
--mainReadFileIntoJsonString f = do
--  result <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
--  case result of
--    Right r -> do
--      let s = BS.unpack r
--      return (Right s)
--    Left _ -> return (Left "File read error.")
--
--mainRosterOrQuit :: FilePath -> TVarPlayers -> IO (RosterName, RosterYear)
--mainRosterOrQuit fp TVarPlayers = do
--  rosterStringEither :: Either ErrorString JsonString <- mainReadFileIntoJsonString fp
--  case rosterStringEither of
--    Right rs -> do
--      let maybeRoster :: Maybe Roster = rosterJsonStringToRoster rs
--      case maybeRoster of
--        Just r -> do
--          STM.atomically $ writeSTM.TVar TVarPlayers (players r)
--          return (rosterName r, rosterYear r)
--        Nothing -> exitWith (ExitFailure 42)
--    Left _ -> exitWith (ExitFailure 99)
--
--mainDrawPuck :: Hat -> IO (Maybe PlayerSymbol)
--mainDrawPuck hat =
--  if Set.null hat
--    then return Nothing
--    else do
--      n <- randomRIO (0, Prelude.length hat - 1)
--      return (Just (Set.elemAt n hat))
--
--mainStartNewYear :: TVarGiftYear -> TVarPlayers -> TVarGiverHat -> TVarGiveeHat -> TVarMaybeGiver -> TVarMaybeGivee -> TVarDiscards -> IO ()
--mainStartNewYear TVarGiftYear TVarPlayers TVarGiverHat TVarGiveeHat TVarMaybeGiver TVarMaybeGivee TVarDiscards = do
--  plrs <- STM.readTVarIO TVarPlayers
--  let nhgr = hatMakeHat plrs
--  let nhge = hatMakeHat plrs
--  mgr <- mainDrawPuck nhgr
--  mge <- mainDrawPuck nhge
--  STM.atomically $ modifySTM.TVar TVarGiftYear (+ 1)
--  STM.atomically $ modifySTM.TVar TVarPlayers playersAddYear
--  STM.atomically $ writeSTM.TVar TVarGiverHat nhgr
--  STM.atomically $ writeSTM.TVar TVarGiveeHat nhge
--  STM.atomically $ writeSTM.TVar TVarMaybeGiver mgr
--  STM.atomically $ writeSTM.TVar TVarMaybeGivee mge
--  STM.atomically $ writeSTM.TVar TVarDiscards Set.empty
--
--mainSelectNewGiver :: TVarMaybeGiver -> TVarGiverHat -> TVarGiveeHat -> TVarDiscards -> TVarMaybeGivee -> IO ()
--mainSelectNewGiver TVarMaybeGiver TVarGiverHat TVarGiveeHat TVarDiscards TVarMaybeGivee = do
--  mgr <- STM.readTVarIO TVarMaybeGiver
--  STM.atomically $ modifySTM.TVar TVarGiverHat (hatRemovePuck (fromJust mgr))
--  dc <- STM.readTVarIO TVarDiscards
--  STM.atomically $ modifySTM.TVar TVarGiveeHat (hatReturnDiscards dc)
--  STM.atomically $ writeSTM.TVar TVarDiscards Set.empty
--  grh <- STM.readTVarIO TVarGiverHat
--  mgr1 <- mainDrawPuck grh
--  STM.atomically $ writeSTM.TVar TVarMaybeGiver mgr1
--  geh <- STM.readTVarIO TVarGiveeHat
--  mge <- mainDrawPuck geh
--  STM.atomically $ writeSTM.TVar TVarMaybeGivee mge
--
--mainGiveeIsSuccess :: TVarMaybeGiver -> TVarGiftYear -> TVarMaybeGivee -> TVarPlayers -> TVarGiveeHat -> IO ()
--mainGiveeIsSuccess TVarMaybeGiver TVarGiftYear TVarMaybeGivee TVarPlayers TVarGiveeHat = do
--  mgr <- STM.readTVarIO TVarMaybeGiver
--  gy <- STM.readTVarIO TVarGiftYear
--  mge <- STM.readTVarIO TVarMaybeGivee
--  STM.atomically $ modifySTM.TVar TVarPlayers (playersUpdateGivee (fromJust mgr) (fromJust mge) gy)
--  STM.atomically $ modifySTM.TVar TVarPlayers (playersUpdateGiver (fromJust mge) (fromJust mgr) gy)
--  STM.atomically $ modifySTM.TVar TVarGiveeHat (hatRemovePuck (fromJust mge))
--  STM.atomically $ writeSTM.TVar TVarMaybeGivee Nothing
--
--mainGiveeIsFailure :: TVarMaybeGivee -> TVarGiveeHat -> TVarDiscards -> IO ()
--mainGiveeIsFailure TVarMaybeGivee TVarGiveeHat TVarDiscards = do
--  mge <- STM.readTVarIO TVarMaybeGivee
--  STM.atomically $ modifySTM.TVar TVarGiveeHat (hatRemovePuck (fromJust mge))
--  STM.atomically $ modifySTM.TVar TVarDiscards (hatDiscardGivee (fromJust mge))
--  geh <- STM.readTVarIO TVarGiveeHat
--  mge1 <- mainDrawPuck geh
--  STM.atomically $ writeSTM.TVar TVarMaybeGivee mge1
--
--mainErrorListIsEmpty :: TVarPlayers -> TVarGiftYear -> IO Bool
--mainErrorListIsEmpty TVarPlayers TVarGiftYear = do
--  plrs <- STM.readTVarIO TVarPlayers
--  gy <- STM.readTVarIO TVarGiftYear
--  let plrKeys = Map.keys plrs
--  let errorList = [plrSymbol | plrSymbol <- plrKeys, let geeCode = playersGetGivee plrSymbol plrs gy, let gerCode = playersGetGiver plrSymbol plrs gy, (plrSymbol == gerCode) || (plrSymbol == geeCode)]
--  return (null errorList)
--
--mainPrintResults :: TVarPlayers -> TVarGiftYear -> IO ()
--mainPrintResults TVarPlayers TVarGiftYear = do
--  plrs <- STM.readTVarIO TVarPlayers
--  gy <- STM.readTVarIO TVarGiftYear
--  errorListIsEmpty <- mainErrorListIsEmpty TVarPlayers TVarGiftYear
--  let plrKeys = Map.keys plrs
--  mapM_
--    putStrLn
--    [ do
--        let pn = playersGetPlayerName plrSymbol plrs
--        let geeCode = playersGetGivee plrSymbol plrs gy
--        let geeName = playersGetPlayerName geeCode plrs
--        let gerCode = playersGetGiver plrSymbol plrs gy
--        if (plrSymbol == geeCode) && (plrSymbol == gerCode)
--          then pn ++ " is neither **buying** for nor **receiving** from anyone - **ERROR**"
--          else
--            if plrSymbol == gerCode
--              then pn ++ " is **receiving** from no one - **ERROR**"
--              else
--                if plrSymbol == geeCode
--                  then pn ++ " is **buying** for no one - **ERROR**"
--                  else pn ++ " is buying for " ++ geeName
--      | plrSymbol <- plrKeys
--    ]
--  unless errorListIsEmpty $ do
--    putStrLn "\nThere is a logic error in this year's pairings."
--    putStrLn "Do you see how it occurs?"
--    putStrLn "If not... call me and I'll explain!"
--
--mainPrintStringGivingRoster :: RosterName -> RosterYear -> TVarGiftYear -> TVarPlayers -> IO ()
--mainPrintStringGivingRoster rn ry TVarGiftYear TVarPlayers = do
--  gy <- STM.readTVarIO TVarGiftYear
--  putStrLn ("\n" ++ rn ++ " - Year " ++ show (ry + gy) ++ " Gifts:\n")
--  mainPrintResults TVarPlayers TVarGiftYear
--
--mainPromptLine :: String -> IO String
--mainPromptLine prompt = do
--  putStr prompt
--  hFlush stdout
--  getLine
--
--mainPrintAndAsk :: RosterName -> RosterYear -> TVarGiftYear -> TVarPlayers -> IO String
--mainPrintAndAsk rn ry TVarGiftYear TVarPlayers = do
--  mainPrintStringGivingRoster rn ry TVarGiftYear TVarPlayers
--  mainPromptLine "\nContinue? ('q' to quit): "

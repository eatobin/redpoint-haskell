module Main where

import           All_Tests
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Loops    (whileM_)
import qualified Data.ByteString.Char8  as BS
import           Data.Char
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           Hat
import           Hat_Test
import           Roster
import           Roster_Test
import           Roster_Utility
import           Rules
import           Rules_Test
import           System.Directory
import           System.Random

type TVGYear = TVar GYear
type TVPlayersMap = TVar PlayersMap
type TVGiverHat = TVar GiverHat
type TVGiveeHat = TVar GiveeHat
type TVGiver = TVar (Maybe Giver)
type TVGivee = TVar (Maybe Givee)
type TVDiscards = TVar Discards

main :: IO ()
main = do
  tvGY <- atomically (newTVar 0)
  tvGiver <- atomically (newTVar (Just "none"))
  tvGivee <- atomically (newTVar (Just "none"))
  rosterList <- makeRosterList <$> readFileIntoString "blackhawks2010.txt"
  let rName = getRosterName rosterList
  let rYear = getRosterYear rosterList
  tvPM <- atomically $ newTVar $ makePlayersMap rosterList
  tvGiverHat <- atomically (newTVar [])
  tvGiveeHat <- atomically (newTVar [])
  tvDiscards <- atomically (newTVar [])
  whileM_ ((/= "q") <$> map toLower <$> printAndAsk rName rYear tvGY tvPM) $ do
    startNewYear tvGY tvPM tvGiverHat tvGiveeHat tvGiver tvGivee tvDiscards
    whileM_ (fmap isJust (readTVarIO tvGiver)) $ do
      whileM_ (fmap isJust (readTVarIO tvGivee)) $ do
        gr <- readTVarIO tvGiver
        ge <- readTVarIO tvGivee
        gy <- readTVarIO tvGY
        pm <- readTVarIO tvPM
        if
          giveeNotSelf (fromJust gr) (fromJust ge) &&
          giveeNotRecip (fromJust gr) (fromJust ge) gy pm &&
          giveeNotRepeat (fromJust gr) (fromJust ge) gy pm
        then
          giveeIsSuccess tvGiver tvGY tvGivee tvPM tvGiveeHat
        else
          giveeIsFailure tvGivee tvGiveeHat tvDiscards
      selectNewGiver tvGiver tvGiverHat tvDiscards tvGiveeHat tvGivee
    putStrLn ""
  putStrLn ""
  putStrLn "This was fun!"
  putStrLn "Talk about a position with Redpoint?"
  putStrLn "Please call: Eric Tobin 773-325-1516"
  putStrLn "Thanks! Bye..."
  putStrLn ""

drawPuckGivee :: GiveeHat -> IO (Maybe Givee)
drawPuckGivee [] = return Nothing
drawPuckGivee geh = Just . (geh !!) <$> (randomRIO (0, length geh -1))

drawPuckGiver :: GiverHat -> IO (Maybe Giver)
drawPuckGiver [] = return Nothing
drawPuckGiver grh = Just . (grh !!) <$> (randomRIO (0, length grh -1))

readFileIntoString :: FilePath -> IO String
readFileIntoString f = do
  dfe <- doesFileExist f
  if dfe
    then do
      bs <- BS.readFile f
      let s = BS.unpack bs
      return s
    else do
      let bs = BS.empty
          s = BS.unpack bs
      return s

startNewYear :: TVGYear -> TVPlayersMap -> TVGiverHat -> TVGiveeHat -> TVGiver -> TVGivee -> TVDiscards -> IO ()
startNewYear tvGY tvPM tvGiverHat tvGiveeHat tvGiver tvGivee tvDiscards = do
  pm <- readTVarIO tvPM
  let nhgr = makeHatGiver pm
  let nhge = makeHatGivee pm
  gr <- drawPuckGiver nhgr
  ge <- drawPuckGivee nhge
  atomically $ modifyTVar tvGY (+1)
  atomically $ modifyTVar tvPM addYearInRoster
  atomically $ writeTVar tvGiverHat nhgr
  atomically $ writeTVar tvGiveeHat nhge
  atomically $ writeTVar tvGiver gr
  atomically $ writeTVar tvGivee ge
  atomically $ modifyTVar tvDiscards emptyDiscards

selectNewGiver :: TVGiver -> TVGiverHat -> TVDiscards -> TVGiveeHat -> TVGivee -> IO ()
selectNewGiver tvGiver tvGiverHat tvDiscards tvGiveeHat tvGivee = do
  gr <- readTVarIO tvGiver
  dc <- readTVarIO tvDiscards
  atomically $ modifyTVar tvGiverHat (removePuckGiver (fromJust gr))
  atomically $ modifyTVar tvGiveeHat (returnDiscards dc)
  atomically $ modifyTVar tvDiscards emptyDiscards
  grh <- readTVarIO tvGiverHat
  gr <- drawPuckGiver grh
  atomically $ writeTVar tvGiver gr
  geh <- readTVarIO tvGiveeHat
  ge <- drawPuckGivee geh
  atomically $ writeTVar tvGivee ge

giveeIsSuccess :: TVGiver -> TVGYear -> TVGivee -> TVPlayersMap -> TVGiveeHat-> IO ()
giveeIsSuccess tvGiver tvGY tvGivee tvPM tvGiveeHat = do
  gr <- readTVarIO tvGiver
  gy <- readTVarIO tvGY
  ge <- readTVarIO tvGivee
  atomically $ modifyTVar tvPM (setGiveeInRoster (fromJust gr) gy (fromJust ge))
  atomically $ modifyTVar tvPM (setGiverInRoster (fromJust ge) gy (fromJust gr))
  atomically $ modifyTVar tvGiveeHat (removePuckGivee (fromJust ge))
  atomically $ writeTVar tvGivee Nothing

giveeIsFailure :: TVGivee -> TVGiveeHat -> TVDiscards -> IO ()
giveeIsFailure tvGivee tvGiveeHat tvDiscards = do
  ge <- readTVarIO tvGivee
  geh <- readTVarIO tvGiveeHat
  atomically $ modifyTVar tvGiveeHat (removePuckGivee (fromJust ge))
  atomically $ modifyTVar tvDiscards (discardPuckGivee (fromJust ge))
  geh <- readTVarIO tvGiveeHat
  ge <- drawPuckGivee geh
  atomically $ writeTVar tvGivee ge

promptLine :: String -> IO String
promptLine prompt = do
  putStr prompt
  getLine

printAndAsk :: RName -> RYear -> TVGYear -> TVPlayersMap -> IO String
printAndAsk rn ry tvGY tvPM = do
  printGivingRoster rn ry tvGY tvPM
  line <- promptLine "\nContinue? ('q' to quit): "
  return line

printGivingRoster :: RName -> RYear -> TVGYear -> TVPlayersMap -> IO ()
printGivingRoster rn ry tvGY tvPM = do
  gy <- readTVarIO tvGY
  pm <- readTVarIO tvPM
  putStrLn ("\n" ++ rn ++ " - Year " ++ show (ry + gy) ++ " Gifts:\n")
  mapM_ putStrLn
    [ n ++ " is buying for " ++  gen
      |          let xs = Map.keys pm,
        x <- xs, let n = getPlayerNameInRoster x pm,
                 let ge = getGiveeInRoster x pm gy,
                 let gen = getPlayerNameInRoster ge pm,
                 ge /= "none" ]

  let errors = not (null [ x | let xs = Map.keys pm, x <- xs, let ge = getGiveeInRoster x pm gy, ge == "none" ])
  when errors $ putStrLn "\nThere is a logic error in this year's pairings.\nDo you see it?\nIf not... call me and I'll explain!\n"

  mapM_ putStrLn
    [ n ++ " is buying for no one."
      |          let xs = Map.keys pm,
        x <- xs, let n = getPlayerNameInRoster x pm,
                 let ge = getGiveeInRoster x pm gy,
                 ge == "none" ]

  mapM_ putStrLn
    [ n ++ " is receiving from no one."
      |          let xs = Map.keys pm,
        x <- xs, let n = getPlayerNameInRoster x pm,
                 let gr = getGiverInRoster x pm gy,
                 gr == "none" ]

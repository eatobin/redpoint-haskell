module Main where

import           All_Tests
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString.Char8  as BS
import qualified Data.Map.Strict        as Map
import           Hat
import           Hat_Test
import           Roster
import           Roster_Test
import           Roster_Utility
import           Rules
import           Rules_Test
import           System.Directory

type TVGYear = TVar GYear
type TVPlayersMap = TVar PlayersMap
type TVGiverHat = TVar GiverHat
type TVGiveeHat = TVar GiveeHat
type TVGiver = TVar Giver
type TVGivee = TVar Givee
type TVDiscards = TVar Discards

main :: IO ()
main = do
  tvGY <- atomically (newTVar 0)
  tvGiver <- atomically (newTVar "none")
  tvGivee <- atomically (newTVar "none")
  rosterList <- makeRosterList <$> readFileIntoString "beatles2014.txt"
  let rName = getRosterName rosterList
  let rYear = getRosterYear rosterList
  tvPM <- atomically $ newTVar $ makePlayersMap rosterList
  tvGiverHat <- atomically (newTVar [])
  tvGiveeHat <- atomically (newTVar [])
  tvDiscards <- atomically (newTVar [])
    -- gy <- readTVarIO tvGY
    --let gy = 0
    -- atomically $ modifyTVar tvRosterPrintString printStringRoster
    -- rosterPrintString <- readTVarIO tvRosterPrintString
    -- atomically $ writeTVar tvRosterPrintString ("\n" ++ rName ++ " - Year " ++ show rYear ++ " Gifts:\n\n")
    -- atomically $ writeTVar tvRosterPrintString (printStringRoster rName rYear 0 playersMap)
    --  atomically $ writeTVar tvRosterPrintString printStringRoster

    --atomically $ modifyTVar tvGY ((+1) tvGY)
    -- print rName
    -- roster <- readTVarIO tvPM
    -- print roster
    -- -- print rYear
    -- -- print playersMap
    -- print (getPlayerInRoster "RinSta" roster)
    -- print (getPlayerNameInRoster "RinSta" roster)
    -- print (getGiveeInRoster "PauMcc" roster gy)
    -- atomically $ modifyTVar tvPM (setGiveeInRoster "PauMcc" gy "PauMcc")
    --roster <- readTVarIO tvPM
    -- print (getGiveeInRoster "PauMcc" roster gy)
    -- print (getGiverInRoster "PauMcc" roster gy)
    -- atomically $ modifyTVar tvPM (setGiverInRoster "PauMcc" gy "PauMcc")
    -- roster <- readTVarIO tvPM
    -- print (getGiverInRoster "PauMcc" roster gy)
    -- atomically $ modifyTVar tvPM addYearInRoster
    -- roster <- readTVarIO tvPM
    -- -- noGivee <- readTVarIO tvNoGivee
    -- -- noGiver <- readTVarIO tvNoGiver
    -- -- rosterPrintString <- readTVarIO tvRosterPrintString
    --giver <- readTVarIO tvGiver
    --givee <- readTVarIO tvGivee
    --print roster
    -- giverHat <- readTVarIO tvGiverHat
    -- giveeHat <- readTVarIO tvGiveeHat
    -- giver <- readTVarIO tvGivee
    -- givee <- readTVarIO tvGiver
    -- print gy
    -- print roster
    -- print giverHat
    -- print giveeHat
    -- print giver
    -- print givee
  startNewYear tvGY tvPM tvGiverHat tvGiveeHat tvGiver tvGivee tvDiscards
  giverHat <- readTVarIO tvGiverHat
  giveeHat <- readTVarIO tvGiveeHat
  giver <- readTVarIO tvGiver
  givee <- readTVarIO tvGivee
  -- print giverHat
  print giveeHat
  print givee
  selectNewgiver tvGiver tvGiverHat tvDiscards tvGiveeHat tvGivee
    -- giverHat <- readTVarIO tvGiverHat
  giveeHat <- readTVarIO tvGiveeHat
    -- giver <- readTVarIO tvGiver
  givee <- readTVarIO tvGivee
    -- print giverHat
  print giveeHat
  print givee
    -- gy <- readTVarIO tvGY
    -- roster <- readTVarIO tvPM
    -- giverHat <- readTVarIO tvGiverHat
    -- giveeHat <- readTVarIO tvGiveeHat
    -- giver <- readTVarIO tvGivee
    -- givee <- readTVarIO tvGiver
    -- discards <- readTVarIO tvDiscards
    -- print gy
    -- print roster
    -- print giverHat
    -- -- print giveeHat
    -- print giver
    -- print givee
    -- print discards
    --print giver
    --print givee
    -- printGivingRoster rName rYear tvGY tvPM
    --startNewYear gy
  print "Bye"


--startNewYear :: TVGYear -> STM ()
--startNewYear tvGY = do
--  gy <- readTVar tvGY
--  writeTVar tvGY (gy + 1)

-- createGiftYear :: GYear -> IO TVGYear
-- createGiftYear gy = newTVarIO gy

-- incrementGYear :: TVGYear -> IO ()
-- incrementGYear tvgy = do
--   gy <- readTVarIO tvgy
--   atomically $ writeTVar tvgy (gy + 1)

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

selectNewgiver :: TVGiver -> TVGiverHat -> TVDiscards -> TVGiveeHat -> TVGivee -> IO ()
selectNewgiver tvGiver tvGiverHat tvDiscards tvGiveeHat tvGivee = do
  gr <- readTVarIO tvGiver
  dc <- readTVarIO tvDiscards
  atomically $ modifyTVar tvGiverHat (removePuckGiver gr)
  atomically $ modifyTVar tvGiveeHat (returnDiscards dc)
  grh <- readTVarIO tvGiverHat
  gr <- drawPuckGiver grh
  atomically $ writeTVar tvGiver gr
  geh <- readTVarIO tvGiveeHat
  ge <- drawPuckGiver geh
  atomically $ writeTVar tvGivee ge

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

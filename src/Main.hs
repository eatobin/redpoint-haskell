module Main where

import           All_Tests
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.Map.Strict        as Map
import           Hat
import           Hat_Test
import           Roster
import           Roster_Test
import           Roster_Utility
import           Rules
import           Rules_Test

type TVGYear = TVar GYear
type TVPlayersMap = TVar PlayersMap
type TVGiverHat = TVar Hat
type TVGiveeHat = TVar Hat
type TVGiver = TVar Giver
type TVGivee = TVar Givee

main :: IO ()
main = do
    tvGY <- atomically (newTVar 0)
    tvGiver <- atomically (newTVar "none")
    tvGivee <- atomically (newTVar "none")
    rosterString <- readFile "beatles2014.txt"
    let rosterList = makeRosterList rosterString
    let rosterInfo = makeRosterInfo rosterList
    let playersList = makePlayersList rosterList
    let rName = getRosterName rosterInfo
    let rYear = getRosterYear rosterInfo
    let playersMap = makePlayersMap rosterList
    tvRoster <- atomically (newTVar playersMap)
    tvGiverHat <- atomically (newTVar [])
    tvGiveeHat <- atomically (newTVar [])
    gy <- readTVarIO tvGY
    --let gy = 0
    -- atomically $ modifyTVar tvRosterPrintString printStringRoster
    -- rosterPrintString <- readTVarIO tvRosterPrintString
    -- atomically $ writeTVar tvRosterPrintString ("\n" ++ rName ++ " - Year " ++ show rYear ++ " Gifts:\n\n")
    -- atomically $ writeTVar tvRosterPrintString (printStringRoster rName rYear 0 playersMap)
    --  atomically $ writeTVar tvRosterPrintString printStringRoster

    --atomically $ modifyTVar tvGY ((+1) tvGY)
    -- print rName
    -- roster <- readTVarIO tvRoster
    -- print roster
    -- -- print rYear
    -- -- print playersMap
    -- print (getPlayerInRoster "RinSta" roster)
    -- print (getPlayerNameInRoster "RinSta" roster)
    -- print (getGiveeInRoster "PauMcc" roster gy)
    -- atomically $ modifyTVar tvRoster (setGiveeInRoster "PauMcc" gy "PauMcc")
    --roster <- readTVarIO tvRoster
    -- print (getGiveeInRoster "PauMcc" roster gy)
    -- print (getGiverInRoster "PauMcc" roster gy)
    -- atomically $ modifyTVar tvRoster (setGiverInRoster "PauMcc" gy "PauMcc")
    -- roster <- readTVarIO tvRoster
    -- print (getGiverInRoster "PauMcc" roster gy)
    -- atomically $ modifyTVar tvRoster addYearInRoster
    -- roster <- readTVarIO tvRoster
    -- -- noGivee <- readTVarIO tvNoGivee
    -- -- noGiver <- readTVarIO tvNoGiver
    -- -- rosterPrintString <- readTVarIO tvRosterPrintString
    --giver <- readTVarIO tvGiver
    --givee <- readTVarIO tvGivee
    --print roster
    giverHat <- readTVarIO tvGiverHat
    giveeHat <- readTVarIO tvGiveeHat
    giver <- readTVarIO tvGivee
    givee <- readTVarIO tvGiver
    print gy
    print roster
    print giverHat
    print giveeHat
    print giver
    print givee
    startNewYear tvGY tvRoster tvGiverHat tvGiveeHat tvGiver tvGivee
    gy <- readTVarIO tvGY
    roster <- readTVarIO tvRoster
    giverHat <- readTVarIO tvGiverHat
    giveeHat <- readTVarIO tvGiveeHat
    giver <- readTVarIO tvGivee
    givee <- readTVarIO tvGiver
    print gy
    print roster
    print giverHat
    print giveeHat
    print giver
    print givee
    --print giver
    --print givee
    --printGivingRoster rName rYear gy tvRoster
    --startNewYear gy
    -- print "Bye"


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

startNewYear :: TVGYear -> TVPlayersMap -> TVGiverHat -> TVGiveeHat -> TVGiver -> TVGivee -> IO ()
startNewYear tvGY tvRoster tvGiverHat tvGiveeHat tvGiver tvGivee = do
  roster <- readTVarIO tvRoster
  let nhgr = makeHat roster
  let nhge = makeHat roster
  gr <- drawPuck nhgr
  ge <- drawPuck nhge
  atomically $ modifyTVar tvGY (+1)
  atomically $ modifyTVar tvRoster addYearInRoster
  atomically $ writeTVar tvGiverHat nhgr
  atomically $ writeTVar tvGiveeHat nhge
  atomically $ writeTVar tvGiver gr
  atomically $ writeTVar tvGivee ge

printGivingRoster :: RName -> RYear -> GYear -> TVPlayersMap -> IO ()
printGivingRoster rn ry gy tvpm = do
  pm <- readTVarIO tvpm
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

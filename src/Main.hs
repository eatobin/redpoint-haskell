module Main where

import           All_Tests
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.Map.Strict        as Map
import           Roster
import           Roster_Test
import           Roster_Utility
import Hat

main :: IO ()
main = do
    rosterString <- readFile "beatles2014.txt"
    let rosterList = makeRosterList rosterString
    let rosterInfo = makeRosterInfo rosterList
    let playersList = makePlayersList rosterList
    let rName = getRosterName rosterInfo
    let rYear = getRosterYear rosterInfo
    let playersMap = makePlayersMap rosterList
    let gy = 0
    -- atomically $ modifyTVar tvRosterPrintString printStringRoster
    -- rosterPrintString <- readTVarIO tvRosterPrintString
    -- atomically $ writeTVar tvRosterPrintString ("\n" ++ rName ++ " - Year " ++ show rYear ++ " Gifts:\n\n")
    -- atomically $ writeTVar tvRosterPrintString (printStringRoster rName rYear 0 playersMap)
    --  atomically $ writeTVar tvRosterPrintString printStringRoster
    tvRoster <- atomically (newTVar playersMap)
    -- print rName
    -- roster <- readTVarIO tvRoster
    -- print roster
    -- -- print rYear
    -- -- print playersMap
    -- print (getPlayerInRoster "RinSta" roster)
    -- print (getPlayerNameInRoster "RinSta" roster)
    -- print (getGiveeInRoster "PauMcc" roster gy)
    -- atomically $ modifyTVar tvRoster (setGiveeInRoster "PauMcc" gy "PauMcc")
    -- roster <- readTVarIO tvRoster
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
    -- print roster
    printGivingRoster rName rYear gy tvRoster


printGivingRoster :: RName -> RYear -> GYear -> TVar (PlayersMap) -> IO ()
printGivingRoster rn ry gy tvpm =
  do
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

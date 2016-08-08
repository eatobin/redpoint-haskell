module Main where

import           All_Tests
import           Control.Concurrent.STM
import           Roster
import           Roster_Test
import           Roster_Utility
import           System.IO

--main = do
--    let m0 = Map.empty
--    let m1 = Map.insert "k1" 7 m0
--    let m  = Map.insert "k2" 13 m1
--    putStrLn $ "map: " ++ show m

--    let v1 = m ! "k1"
--    putStrLn $ "v1: " ++ show v1
--    putStrLn $ "len: " ++ show (Map.size m)

--    let m' = Map.delete "k2" m
--    putStrLn $ "map: " ++ show m'

--    let prs = Map.lookup "k2" m'
--    putStrLn $ "prs: " ++ show prs

--    let n = Map.fromList [("foo", 1), ("bar", 2)]
--    putStrLn $ "map: " ++ show n

--m9 = Map.empty
--m10 = Map.insert "k1" 77 m9
--m11 = Map.insert "k2" 133 m10

--myMap :: Ord k => [(k, a)] -> Map k a
--myMap = Map.fromList

main :: IO ()
main = do
    rosterString <- readFile "beatles2014.txt"
    tvNoGivee <- atomically (newTVar [])
    tvNoGiver <- atomically (newTVar [])
    tvRosterPrintString <- atomically (newTVar "")
    let rosterList = makeRosterList rosterString
    let rosterInfo = makeRosterInfo rosterList
    let playersList = makePlayersList rosterList
    let rName = getRosterName rosterInfo
    let rYear = getRosterYear rosterInfo
    let playersMap = makePlayersMap rosterList
    tvRoster <- atomically (newTVar playersMap)
    -- print rName
    roster <- readTVarIO tvRoster
    -- print roster
    -- print rYear
    -- print playersMap
    -- print (getPlayerInRoster "RinSta" roster)
    -- print (getPlayerNameInRoster "RinSta" roster)
    -- print (getGiveeInRoster "PauMcc" roster 0)
    -- atomically $ modifyTVar tvRoster (setGiveeInRoster "PauMcc" 0 "PauMcc")
    -- roster <- readTVarIO tvRoster
    -- print (getGiveeInRoster "PauMcc" roster 0)
    -- print (getGiverInRoster "PauMcc" roster 0)
    -- atomically $ modifyTVar tvRoster (setGiverInRoster "PauMcc" 0 "PauMcc")
    -- roster <- readTVarIO tvRoster
    -- print (getGiverInRoster "PauMcc" roster 0)
    atomically $ modifyTVar tvRoster addYearInRoster
    roster <- readTVarIO tvRoster
    noGivee <- readTVarIO tvNoGivee
    noGiver <- readTVarIO tvNoGiver
    rosterPrintString <- readTVarIO tvRosterPrintString
    print roster




--statusToString :: Books -> Borrowers -> String
--statusToString bksb brsb = "\n" ++
--  "--- Status Report of Test Library ---\n" ++
--  "\n" ++
--  libraryToString bksb brsb ++
--  "\n" ++
--  unlines (map bookToString bks) ++ "\n" ++
--  unlines (map borrowerToString brs) ++ "\n" ++
--  "--- End of Status Report ---" ++
--  "\n"
--    where bks = fst bksb
--          brs = fst brsb
  
  
--  (defn print-string-giving-roster [gift-year]
--  (let [no-givee (atom [])
--        no-giver (atom [])
--        roster-string (atom [])]
--    (swap! roster-string conj team-name " - Year " (+ first-year gift-year) " Gifts:\n\n")
--    (doseq [p (keys (into (sorted-map) (deref roster)))]
--      (let [player-name (get-player-name p)
--            givee-code (get-givee-code p gift-year)
--            giver-code (get-giver-code p gift-year)]
--        (if (= givee-code :none)
--          (swap! no-givee conj p)
--          (swap! roster-string conj player-name " is buying for " (get-player-name givee-code) "\n"))
--        (if (= giver-code :none)
--          (swap! no-giver conj p))))
--    (if-not (and (empty? (deref no-givee))
--                 (empty? (deref no-giver)))
--      (do
--        (swap! roster-string conj "\nThere is a logic error in this year's pairings.\nDo you see it?\nIf not... call me and I'll explain!\n\n")
--        (doseq [p (deref no-givee)]
--          (swap! roster-string conj (get-player-name p) " is buying for no one.\n"))
--        (doseq [p (deref no-giver)]
--          (swap! roster-string conj (get-player-name p) " is receiving from no one.\n"))))
--    (apply str (deref roster-string))))

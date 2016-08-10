module Main where

import           All_Tests
import           Control.Concurrent.STM
import           Roster
import           Roster_Test
import           Roster_Utility
import           System.IO
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

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
    -- atomically $ modifyTVar tvRosterPrintString printStringRoster
    -- rosterPrintString <- readTVarIO tvRosterPrintString
    -- atomically $ writeTVar tvRosterPrintString ("\n" ++ rName ++ " - Year " ++ show rYear ++ " Gifts:\n\n")
    -- atomically $ writeTVar tvRosterPrintString (printStringRoster rName rYear 0 playersMap)
    --  atomically $ writeTVar tvRosterPrintString printStringRoster
    -- tvRoster <- atomically (newTVar playersMap)
    -- print rName
    -- roster <- readTVarIO tvRoster
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
    -- atomically $ modifyTVar tvRoster addYearInRoster
    -- roster <- readTVarIO tvRoster
    -- noGivee <- readTVarIO tvNoGivee
    -- noGiver <- readTVarIO tvNoGiver
    rosterPrintString <- readTVarIO tvRosterPrintString
    -- print roster
    -- print show noGivee
    -- print noGiver
    putStrLn rosterPrintString


-- printStatus :: TVar ([Book], Bool) -> TVar ([Borrower], Bool) -> IO ()
-- printStatus tvbksb tvbrsb = do
--   bksb <- readTVarIO tvbksb
--   brsb <- readTVarIO tvbrsb
--   if snd bksb && snd brsb then putStrLn (statusToString bksb brsb)
--     else putStrLn "\n*** There was an error with the operation just performed! ***\n"




-- printStringRoster :: RName -> RYear -> GYear -> Map PlrSym Player -> String
-- printStringRoster rName rYear gy pm = "\n" ++ rName ++ " - Year " ++ show rYear ++ " Gifts:\n\n"
 -- "--- Status Report of Test Library ---\n" ++
 -- "\n" ++
 -- libraryToString bksb brsb ++
 -- "\n" ++
 -- unlines (map bookToString bks) ++ "\n" ++
 -- unlines (map borrowerToString brs) ++ "\n" ++
 -- "--- End of Status Report ---" ++
 -- "\n"
 --   where bks = fst bksb
 --         brs = fst brsb

-- tester :: RName -> RYear -> GYear -> Map PlrSym Player -> IO ()
-- tester rn ry gy pm = do
--   x <- atomically (newTVar ("\n" ++ rn ++ " - Year " ++ show ry ++ " Gifts:\n\n"))
--   atomically $ modifyTVar x (++ "\none")
--   atomically $ modifyTVar x (++ "\ntwo")
--   atomically $ modifyTVar x (++ "\nthree")
--   let n = getPlayerNameInPlayer plr1
--   let ge = getGiveeInPlayer gy plr1
--   let gr = getGiverInPlayer gy plr1
--   atomically $ modifyTVar x (++ "\n" ++ n ++ ge ++ gr)
--   x <- readTVarIO x
--   putStrLn x


--Haskell implementation
fizzbuzz xs =
  mapM_ putStrLn $
    [ if fizz && buzz then "FizzBuzz"
      else if fizz then "Fizz"
      else if buzz then "Buzz"
      else show x
      | x <- xs, let fizz = x `mod` 3 == 0,
                 let buzz = x `mod` 5 == 0 ]


fizzbuzz2 xs gy pm = do
  putStrLn "Hi Eric!"
  mapM_ putStrLn $
    [ if ge == "none" then n ++ " is buying for nobody - see below..."
      else n ++ " is buying for " ++  gen
      | x <- xs, let n = getPlayerNameInRoster x pm,
                 let ge = getGiveeInRoster x pm gy,
                 let gen = getPlayerNameInRoster ge pm ]

fizzbuzz3 rn ry gy pm =
  do
   putStrLn ("\n" ++ rn ++ " - Year " ++ show ry ++ " Gifts:\n")
   mapM_ putStrLn $
     [ if ge == "none" then n ++ " is buying for nobody - see below..."
       else n ++ " is buying for " ++  gen
       |          let xs = Map.keys pm,
         x <- xs, let n = getPlayerNameInRoster x pm,
                  let ge = getGiveeInRoster x pm gy,
                  let gen = getPlayerNameInRoster ge pm ]

-- little roster = do
--   let syms = Map.keys roster
--     do
--       print "x"
--       print "y"
-- map (getGiveeInPlayer 0) (Map.elems roster)

-- printStringRoster :: String -> IO String
-- printStringRoster rosterString = do
--   let rosterList = makeRosterList rosterString
--   let rosterInfo = makeRosterInfo rosterList
--   let playersList = makePlayersList rosterList
--   let rName = getRosterName rosterInfo
--   let rYear = getRosterYear rosterInfo
--   let playersMap = makePlayersMap rosterList
--   let rosterString = "\n" ++ rName ++ " - Year " ++ show rYear ++ " Gifts:\n\n"
-- --   tvNoGiveeX <- atomically (newTVar "Works!")
--   return rosterString


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

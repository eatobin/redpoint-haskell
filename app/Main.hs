module Main (main) where

import qualified Control.Concurrent.STM as STM
import Transfer1

main :: IO ()
main = do
  fromAcct <- STM.atomically (STM.newTVar 200)
  toAcct <- STM.atomically (STM.newTVar 200)
  showBalance fromAcct toAcct
  putStrLn "Transfering $50 from 'FROM' to 'TO'"
  transfer fromAcct toAcct 50
  putStrLn "Done!"
  showBalance fromAcct toAcct

module Main (main) where

import qualified Control.Concurrent.STM as STM
import Transfer1

main :: IO ()
main = do
  from <- STM.atomically (STM.newTVar 200)
  to <- STM.atomically (STM.newTVar 200)
  showBalance from to
  putStrLn "Transfering $50 from 'FROM' to 'TO'"
  transfer from to 50
  putStrLn "Done!"
  showBalance from to

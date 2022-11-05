module Main (main) where

import qualified Control.Concurrent.STM as STM
import Transfer1

main :: IO ()
main = do
  let fromAcctSTMTVarInt = STM.newTVar 375
  fromAcctTVarInt <- STM.atomically fromAcctSTMTVarInt
  toAcctTVarInt <- STM.atomically (STM.newTVar 200)
  showBalance fromAcctTVarInt toAcctTVarInt
  putStrLn "Transfering $50 from 'FROM' to 'TO'"
  transfer fromAcctTVarInt toAcctTVarInt 50
  putStrLn "Done!"
  showBalance fromAcctTVarInt toAcctTVarInt

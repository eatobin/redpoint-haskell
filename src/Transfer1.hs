module Transfer1 (main1) where

import qualified Control.Concurrent.STM as STM

type Account = STM.TVar Int

withdraw :: Account -> Int -> STM.STM ()
withdraw acc amount = do
  bal <- STM.readTVar acc
  STM.check (amount >= 0 && amount <= bal)
  STM.writeTVar acc (bal - amount)

deposit :: Account -> Int -> STM.STM ()
deposit acc amount = do
  bal <- STM.readTVar acc
  STM.check (amount >= 0)
  STM.writeTVar acc (bal + amount)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount =
  STM.atomically
    ( do
        deposit to amount
        withdraw from amount
    )

showAccount :: Account -> IO Int
showAccount = STM.readTVarIO

showBalance :: Account -> Account -> IO ()
showBalance from to = do
  x <- showAccount from
  y <- showAccount to
  putStrLn $ "FROM balance: $" <> show x
  putStrLn $ "TO balance: $" <> show y

main1 :: IO ()
main1 = do
  from <- STM.atomically (STM.newTVar 200)
  to <- STM.atomically (STM.newTVar 200)
  showBalance from to
  putStrLn "Transfering $50 from 'FROM' to 'TO'"
  transfer from to 50
  putStrLn "Done!"
  showBalance from to

module Transfer1 (withdraw, deposit, transfer, showAccount, showBalance) where

import qualified Control.Concurrent.STM as STM

type AccountTVarInt = STM.TVar Int

withdraw :: AccountTVarInt -> Int -> STM.STM ()
withdraw fromAcctTVarInt amountInt = do
  balInt <- STM.readTVar fromAcctTVarInt
  STM.check (amountInt >= 0 && amountInt <= balInt)
  STM.writeTVar fromAcctTVarInt (balInt - amountInt)

deposit :: AccountTVarInt -> Int -> STM.STM ()
deposit toAcctTVarInt amountInt = do
  balInt <- STM.readTVar toAcctTVarInt
  STM.check (amountInt >= 0)
  STM.writeTVar toAcctTVarInt (balInt + amountInt)

transfer :: AccountTVarInt -> AccountTVarInt -> Int -> IO ()
transfer fromAcctTVarInt toAcctTVarInt amountInt =
  STM.atomically
    ( do
        deposit toAcctTVarInt amountInt
        withdraw fromAcctTVarInt amountInt
    )

showAccount :: AccountTVarInt -> IO Int
showAccount = STM.readTVarIO

showBalance :: AccountTVarInt -> AccountTVarInt -> IO ()
showBalance fromAcctTVarInt toAcctTVarInt = do
  x <- showAccount fromAcctTVarInt
  y <- showAccount toAcctTVarInt
  putStrLn $ "FROM balance: $" <> show x
  putStrLn $ "TO balance: $" <> show y

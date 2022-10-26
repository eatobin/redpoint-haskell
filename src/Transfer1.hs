module Transfer1 (withdraw, deposit, transfer, showAccount, showBalance) where

import qualified Control.Concurrent.STM as STM

type AccountTVarInt = STM.TVar Int

withdraw :: AccountTVarInt -> Int -> STM.STM ()
withdraw fromAcctTVarInt amount = do
  bal <- STM.readTVar fromAcctTVarInt
  STM.check (amount >= 0 && amount <= bal)
  STM.writeTVar fromAcctTVarInt (bal - amount)

deposit :: AccountTVarInt -> Int -> STM.STM ()
deposit toAcctTVarInt amount = do
  bal <- STM.readTVar toAcctTVarInt
  STM.check (amount >= 0)
  STM.writeTVar toAcctTVarInt (bal + amount)

transfer :: AccountTVarInt -> AccountTVarInt -> Int -> IO ()
transfer fromAcctTVarInt toAcctTVarInt amount =
  STM.atomically
    ( do
        deposit toAcctTVarInt amount
        withdraw fromAcctTVarInt amount
    )

showAccount :: AccountTVarInt -> IO Int
showAccount = STM.readTVarIO

showBalance :: AccountTVarInt -> AccountTVarInt -> IO ()
showBalance fromAcctTVarInt toAcctTVarInt = do
  x <- showAccount fromAcctTVarInt
  y <- showAccount toAcctTVarInt
  putStrLn $ "FROM balance: $" <> show x
  putStrLn $ "TO balance: $" <> show y

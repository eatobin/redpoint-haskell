module Transfer1 (main1) where

import qualified Control.Concurrent.STM as E

type Account = E.TVar Int

withdraw :: Account -> Int -> E.STM ()
withdraw acc amount = do
  bal <- E.readTVar acc
  E.check (amount >= 0 && amount <= bal)
  E.writeTVar acc (bal - amount)

deposit :: Account -> Int -> E.STM ()
deposit acc amount = do
  bal <- E.readTVar acc
  E.check (amount >= 0)
  E.writeTVar acc (bal + amount)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount =
  E.atomically
    ( do
        deposit to amount
        withdraw from amount
    )

showAccount :: Account -> IO Int
showAccount = E.readTVarIO

showBalance :: Account -> Account -> IO ()
showBalance from to = do
  x <- showAccount from
  y <- showAccount to
  putStrLn $ "FROM balance: $" <> show x
  putStrLn $ "TO balance: $" <> show y

main1 :: IO ()
main1 = do
  from <- E.atomically (E.newTVar 200)
  to <- E.atomically (E.newTVar 200)
  showBalance from to
  putStrLn "Transfering $50 from 'FROM' to 'TO'"
  transfer from to 50
  putStrLn "Done!"
  showBalance from to

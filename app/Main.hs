module Main (main) where

import System.IO

main :: IO ()
--main = do
--  putStrLn "What's your name?"
--  hFlush stdout
--  name <- getLine
--  putStrLn ("Hello " ++ name)

main = putStrLn "What's your name, honey?" >> hFlush stdout >> getLine >>= putStrLn . ("Hello " ++)

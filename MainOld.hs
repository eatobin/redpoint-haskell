{-# LANGUAGE ScopedTypeVariables #-}

module Main (mainDrawPuck, main) where

import qualified Data.Set as Set
import Gift_Pair
import Hat
import qualified System.Random as Ran

mainDrawPuck :: Hat -> IO (Maybe PlayerKey)
mainDrawPuck hat
  | Set.null hat = return Nothing
  | otherwise = do
    i :: Int <- Ran.randomRIO (0, Prelude.length hat - 1)
    return (Just (Set.elemAt i hat))

main :: IO ()
main =
  do
    putStrLn "So sorry, there is an error here."

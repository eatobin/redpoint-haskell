module Main where

import           All_Tests
import           Roster
import           Roster_Test

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

main = do
    let m0 = Map.empty
    let m1 = Map.insert "k1" 7 m0
    let m  = Map.insert "k2" 13 m1
    putStrLn $ "map: " ++ show m

    let v1 = m ! "k1"
    putStrLn $ "v1: " ++ show v1
    putStrLn $ "len: " ++ show (Map.size m)

    let m' = Map.delete "k2" m
    putStrLn $ "map: " ++ show m'

    let prs = Map.lookup "k2" m'
    putStrLn $ "prs: " ++ show prs

    let n = Map.fromList [("foo", 1), ("bar", 2)]
    putStrLn $ "map: " ++ show n

m9 = Map.empty
m10 = Map.insert "k1" 77 m9
m11 = Map.insert "k2" 133 m10

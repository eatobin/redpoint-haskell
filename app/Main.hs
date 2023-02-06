module Main (main) where

import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen1) = random gen
      (secondCoin, newGen2) = random newGen1
      (thirdCoin, _) = random newGen2
   in (firstCoin, secondCoin, thirdCoin)

threeCoinsDemo :: (Bool, Bool, Bool)
threeCoinsDemo =
  threeCoins (mkStdGen 19)

main :: IO ()
main =
  do
    print threeCoinsDemo

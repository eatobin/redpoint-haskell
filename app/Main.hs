module Main (main) where

import Helpers

main :: IO ()
main = do
  result <- helpersReadFileIntoJsonString "resources-test/beatles.json"
  print result

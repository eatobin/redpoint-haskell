module Main (main) where

import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Gift_Pair

type ErrorString = String

jsonFile :: FilePath
jsonFile = "resources/blackhawks.json"

main :: IO ()
main = do
  input <- mainReadFileIntoJsonString jsonFile
  case input of
    Right r -> putStrLn r
    Left e -> putStrLn e

mainReadFileIntoJsonString :: FilePath -> IO (Either ErrorString JsonString)
mainReadFileIntoJsonString f = do
  result <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
  case result of
    Right r -> do
      let s = BS.unpack r
      return (Right s)
    Left _ -> return (Left "File read error.")

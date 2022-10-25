module Main_Helpers (filePath, mainHelpersReadFileIntoJsonString) where

import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Gift_Pair

type ErrorString = String

filePath :: FilePath
filePath = "resources/blackhawks.json"

mainHelpersReadFileIntoJsonString :: FilePath -> IO (Either ErrorString JsonString)
mainHelpersReadFileIntoJsonString f = do
  result <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
  case result of
    Right r -> do
      let s = BS.unpack r
      return (Right s)
    Left _ -> return (Left "File read error.")

module Main (main) where

--import Control.Exception
import qualified Data.ByteString.Char8 as BS

--import Gift_Pair
--import qualified Data.ByteString.Lazy as B

--type ErrorString = String

main :: IO ()
main = do
  input <- BS.readFile jsonFile
  putStrLn (BS.unpack input)

--main :: IO ()
--main = do
--  getJSON <- BS.readFile jsonFile
--  print BS.unpack getJSON
--mainReadFileIntoJsonString :: FilePath -> IO (Either ErrorString JsonString)
--mainReadFileIntoJsonString f = do
--  result <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
--  case result of
--    Right r -> do
--      let s = BS.unpack r
--      return (Right s)
--    Left _ -> return (Left "File read error.")

jsonFile :: FilePath
jsonFile = "resources/blackhawks.json"

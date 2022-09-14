module Players (playersJsonStringToPlayers) where

import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
--import qualified Data.Map.Strict as Map
import Gift_Pair
import Player
import Prelude hiding (lookup)

--type PlrSym = String
type Players = Map PlrSym Player

type JsonString = String

playersJsonStringToPlayers :: JsonString -> Maybe Players
playersJsonStringToPlayers js = A.decodeStrict (BS.pack js) :: Maybe Players

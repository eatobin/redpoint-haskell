{-# LANGUAGE NamedFieldPuns #-}

module Roster where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.List.Utils

type PlrSym = String
type Givee = PlrSym
type Giver = PlrSym

type PlayersString = String
type PlayersList = [[String]]

data GiftPair = GiftPair
  { givee :: Givee
  , giver :: Giver
  } deriving (Show, Eq)

type Name = String
type GiftHist = [GiftPair]

data Player = Player
  { name     :: Name
  , giftHist :: GiftHist
  } deriving (Show, Eq)

--["RinSta","Ringo Starr","JohLen","GeoHar"]

makeGiftPair :: Givee -> Giver -> GiftPair
makeGiftPair = GiftPair

makePlayer :: Name -> GiftHist -> Player
makePlayer = Player

-- makePlayerMap :: [String] -> Map PlrSym Player
-- makePlayerMap [s, n, ge, gr] =
--   Map.singleton s plr
--     where gp = makeGiftPair ge gr
--           plr = makePlayer n [gp]

makePlayerMap' :: [[String]] -> Map PlrSym Player
makePlayerMap' [[s, n, ge, gr]] =
  Map.fromList [(s, plr)]
    where gp = makeGiftPair ge gr
          plr = makePlayer n [gp]
makePlayerMap' _ = Map.fromList [("RinSta",Player {name = "Ringo Starr", giftHist = [GiftPair {givee = "JohLen", giver = "GeoHar"}]})]

-- makePlayersList :: PlayersString -> PlayersList
-- makePlayersList playersString = map (split ", ") playerString
--   where playerString = lines playersString
--
-- makePlayersMap :: PlayersString -> [Map PlrSym Player]
-- makePlayersMap playersString = map makePlayerMap playersList
--   where playersList = makePlayersList playersString

-- makeMap = Map.singleton
-- makeRoster :: [(String, Player)] -> Map String Player
-- makeRoster = Map.fromList

-- instance FromJSON Borrower where
--   parseJSON (Object v) = Borrower <$>
--                          v .: "name" <*>
--                          v .: "max-books"
--   parseJSON _ = error "Can't parse Borrower from YAML/JSON"
--
-- instance ToJSON Borrower where
--   toJSON (Borrower name maxBooks) = object ["name" .= name, "max-books" .= maxBooks]
--
-- makeBorrower :: Name -> MaxBooks -> Borrower
-- makeBorrower = Borrower
--
-- getName :: Borrower -> Name
-- getName Borrower {name} = name
--
-- setName :: Name -> Borrower -> Borrower
-- setName n br@Borrower {name} = br {name = n}
--
-- getMaxBooks :: Borrower -> MaxBooks
-- getMaxBooks Borrower {maxBooks} = maxBooks
--
-- setMaxBooks :: MaxBooks -> Borrower -> Borrower
-- setMaxBooks m br@Borrower {maxBooks} = br {maxBooks = m}
--
-- borrowerToString :: Borrower -> String
-- borrowerToString br = getName br ++ " (" ++ show (getMaxBooks br) ++ " books)"

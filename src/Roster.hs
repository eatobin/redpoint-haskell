{-# LANGUAGE NamedFieldPuns    #-}

module Roster where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

type PlrSym = String
type Giver = PlrSym
type Givee = PlrSym

data GiftPair = GiftPair
  { giver :: Giver
  , givee :: Givee
  } deriving (Show, Eq)

type Name = String
type GiftHist = [GiftPair]

data Player = Player
  { name     :: Name
  , giftHist :: GiftHist
  } deriving (Show, Eq)

--["RinSta" "Ringo Starr" "JohLen" "GeoHar"]

makeGiftPair :: Giver -> Givee -> GiftPair
makeGiftPair = GiftPair

makePlayer :: Name -> GiftHist -> Player
makePlayer = Player

makePlayerMap :: PlrSym -> Name -> Giver -> Givee -> Map PlrSym Player
makePlayerMap s n gr ge =
  Map.singleton s plr
    where gp = makeGiftPair gr ge
          plr = makePlayer n [gp]

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

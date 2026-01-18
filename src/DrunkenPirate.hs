-- module DrunkenPirate (treasureMap) where

-- import Control.Monad.Identity

-- stagger :: Int -> Identity Int
-- stagger p = Identity (p + 2)

-- crawl :: Int -> Identity Int
-- crawl p = Identity (p + 1)

-- -- treasureMap :: Int -> Identity Int
-- -- treasureMap pos =
-- --   stagger pos
-- --     >>= stagger
-- --     >>= crawl

-- treasureMap :: Int -> Identity Int
-- treasureMap pos =
--   crawl pos
--     >>= stagger
--     >>= stagger

-- \$ stack repl --ghc-options -Wno-type-defaults
-- λ> :l src/DrunkenPirate.hs

-- treasureMap 100 -> Identity 105
-- runIdentity (treasureMap 600) -> 605

module DrunkenPirate (treasureMap, treasureMapX) where

import Control.Monad.Identity (Identity (Identity, runIdentity))

newtype Position t = Position t deriving (Show)

stagger :: (Num t) => Position t -> Position t
stagger (Position d) = Position (d + 2)

staggerX :: (Num t) => Identity t -> Identity t
staggerX (Identity d) = Identity {runIdentity = d + 2}

crawl :: (Num t) => Position t -> Position t
crawl (Position d) = Position (d + 1)

crawlX :: (Num t) => Identity t -> Identity t
crawlX (Identity d) = Identity {runIdentity = d + 1}

rtn :: p -> p
rtn x = x

(>>==) :: t1 -> (t1 -> t2) -> t2
x >>== f = f x

treasureMap :: (Num t) => Position t -> Position t
treasureMap posM =
  posM
    >>== crawl
    >>== stagger
    >>== stagger
    >>== rtn

treasureMapX :: Identity Int -> Identity Int
treasureMapX posX =
  posX
    >>== crawlX
    >>== staggerX
    >>== staggerX
    >>== rtn

-- treasureMap (Position 100) -> Position 105
-- treasureMap (Position (100 :: Int))
-- treasureMap (Position (100.8 :: Double))
-- treasureMapX (Identity 100)
-- runIdentity (treasureMapX 600) -> 605

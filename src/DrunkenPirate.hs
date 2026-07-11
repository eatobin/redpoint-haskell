-- haskell
-- -- It passes the value straight through with no extra logic
-- identityExample :: Identity Int
-- identityExample = do
--     x <- Identity 5
--     return (x + 1)

-- haskell
-- -- It automatically passes the integer state from line to line
-- stateExample :: State Int String
-- stateExample = do
--     current <- get      -- Retrieve the current state
--     put (current + 10)  -- Update the state
--     return "Done!"      -- Return a final value

-- Start Code here

-- To rewrite a pure state-passing function using the State monad, you convert functions that manually accept and return a tuple (result, state) into actions that manipulate state implicitly using get and put.

-- Here is a step-by-step example showing a game scorekeeper rewritten from a manual pure function into the State monad.

-- 1. The Manual Pure ApproachIn standard pure code, you must manually pass the integer score into every function and accept a new tuple back. It is easy to accidentally pass the wrong state variable name.

-- haskell

-- A type synonym for clarity
-- type Score = Int

-- -- Manually taking the state and returning the new state
-- scorePointPure :: Score -> (String, Score)
-- scorePointPure currentScore =
--     let newScore = currentScore + 10
--     in ("Point scored!", newScore)

-- bonusPointPure :: Score -> (String, Score)
-- bonusPointPure currentScore =
--     let newScore = currentScore + 50
--     in ("Bonus awarded!", newScore)

-- -- Threading the state through the chain manually
-- runGamePure :: Score -> (String, Score)
-- runGamePure s0 =
--     let (msg1, s1) = scorePointPure s0
--         (msg2, s2) = bonusPointPure s1
--     in (msg1 ++ " " ++ msg2, s2)

-- 2. The State Monad RewriteWith Control.Monad.State, the state is handled entirely behind the scenes inside the monad's plumbing. You use get to read the state and put to overwrite it.

-- haskell

-- import Control.Monad.State

-- type Score = Int

-- -- No manual state arguments or tuple returns in the type
-- scorePointState :: State Score String
-- scorePointState = do
--     currentScore <- get
--     let newScore = currentScore + 10
--     put newScore
--     return "Point scored!"

-- bonusPointState :: State Score String
-- bonusPointState = do
--     currentScore <- get
--     let newScore = currentScore + 50
--     put newScore
--     return "Bonus awarded!"

-- -- Automatic state threading using do-notation
-- runGameState :: State Score String
-- runGameState = do
--     msg1 <- scorePointState
--     msg2 <- bonusPointState
--     return (msg1 ++ " " ++ msg2)

-- 3. How to Execute the State Monad

-- To actually run your State action and get data back, you unwrap it by providing an initial state value to one of three runner functions:
-- runState: Returns both the final value and the final state as a tuple.
-- evalState: Returns only the final computed value.
-- execState: Returns only the final state.

-- haskell

-- main :: IO ()
-- main = do
--     let initialState = 0

--     -- 1. Get both the message and the final score
--     print $ runState runGameState initialState
--     -- Output: ("Point scored! Bonus awarded!", 60)

--     -- 2. Get only the message
--     print $ evalState runGameState initialState
--     -- Output: "Point scored! Bonus awarded!"

--     -- 3. Get only the final score
--     print $ execState runGameState initialState

-- Pro-Tip: Using modify

-- You can make the monadic code even shorter by using modify, which applies a function directly to the internal state without needing separate get and put steps:

-- haskell

-- scorePointState :: State Score String
-- scorePointState = do
--     modify (+ 10)
--     return "Point scored!"

module DrunkenPirate (treasureMap) where

import Control.Monad.Identity (Identity (Identity, runIdentity))

stagger :: Int -> Identity Int
stagger p = Identity {runIdentity = p + 2}

crawl :: Int -> Identity Int
crawl p = Identity {runIdentity = p + 1}

-- treasureMap :: Int -> Identity Int
-- treasureMap pos =
--   stagger pos
--     >>= stagger
--     >>= crawl

treasureMap :: Int -> Identity Int
treasureMap pos =
  crawl pos
    >>= stagger
    >>= stagger

-- \$ stack repl --ghc-options -Wno-type-defaults
-- λ> :l src/DrunkenPirate.hs

-- treasureMap 100 -> Identity 105
-- runIdentity (treasureMap 600) -> 605

-- module DrunkenPirate (treasureMap, treasureMapX) where

-- import Control.Monad.Identity (Identity (Identity, runIdentity))

-- newtype Position t = Position t deriving (Show)

-- stagger :: (Num t) => Position t -> Position t
-- stagger (Position d) = Position (d + 2)

-- staggerX :: (Num t) => Identity t -> Identity t
-- staggerX (Identity d) = Identity {runIdentity = d + 2}

-- crawl :: (Num t) => Position t -> Position t
-- crawl (Position d) = Position (d + 1)

-- crawlX :: (Num t) => Identity t -> Identity t
-- crawlX (Identity d) = Identity {runIdentity = d + 1}

-- rtn :: p -> p
-- rtn x = x

-- (>>==) :: t1 -> (t1 -> t2) -> t2
-- x >>== f = f x

-- treasureMap :: (Num t) => Position t -> Position t
-- treasureMap posM =
--   posM
--     >>== crawl
--     >>== stagger
--     >>== stagger
--     >>== rtn

-- treasureMapX :: Identity Int -> Identity Int
-- treasureMapX posX =
--   posX
--     >>== crawlX
--     >>== staggerX
--     >>== staggerX
--     >>== rtn

-- treasureMap (Position 100) -> Position 105
-- treasureMap (Position (100 :: Int))
-- treasureMap (Position (100.8 :: Double))
-- treasureMapX (Identity 100)
-- runIdentity (treasureMapX 600) -> 605

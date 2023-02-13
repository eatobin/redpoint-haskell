module TurnstileMain (turnstileMain) where

import Control.Monad.State

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

push :: TurnstileState -> (TurnstileOutput, TurnstileState)
push Locked = (Tut, Locked)
push Unlocked = (Open, Locked)

coinS :: State TurnstileState TurnstileOutput
coinS = state coin

pushS :: State TurnstileState TurnstileOutput
pushS = state push

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  a4 <- coinS
  a5 <- pushS
  return [a1, a2, a3, a4, a5]

turnstileMain :: IO ()
turnstileMain =
  do
    print (runState mondayS Locked)

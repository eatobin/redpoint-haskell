module Main (main) where

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

regularPersonPush :: TurnstileState -> (TurnstileOutput, TurnstileState)
regularPersonPush Locked = (Tut, Locked)
regularPersonPush Unlocked = (Open, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = regularPersonPush s1
      (a3, s3) = regularPersonPush s2
      (a4, s4) = coin s3
      (a5, s5) = regularPersonPush s4
   in ([a1, a2, a3, a4, a5], s5)

main :: IO ()
main =
  do
    print (monday Locked)

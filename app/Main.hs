module Main (main) where

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut | Unchanged
  deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

regularPersonPush :: TurnstileState -> (TurnstileOutput, TurnstileState)
regularPersonPush Locked = (Tut, Locked)
regularPersonPush Unlocked = (Open, Locked)

distractedPersonNoPush :: TurnstileState -> (TurnstileOutput, TurnstileState)
distractedPersonNoPush Locked = (Unchanged, Locked)
distractedPersonNoPush Unlocked = (Unchanged, Unlocked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = distractedPersonNoPush s1
      (a3, s3) = regularPersonPush s2
      (a4, s4) = distractedPersonNoPush s3
      (a5, s5) = coin s4
   in ([a1, a2, a3, a4, a5], s5)

main :: IO ()
main =
  do
    print (monday Locked)

module Main (main) where

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Pass | Tut | Wander
  deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

regularPersonPush :: TurnstileState -> (TurnstileOutput, TurnstileState)
regularPersonPush _ = (Pass, Locked)

distractedPersonNoPush :: TurnstileState -> (TurnstileOutput, TurnstileState)
distractedPersonNoPush _ = (Wander, Unlocked)

hastyPersonPush :: TurnstileState -> (TurnstileOutput, TurnstileState)
hastyPersonPush Locked = (Tut, Locked)
hastyPersonPush Unlocked = (Pass, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = regularPersonPush s0
      (a2, s2) = distractedPersonNoPush s1
      (a3, s3) = hastyPersonPush s2
      (a4, s4) = coin s3
      (a5, s5) = hastyPersonPush s4
   in ([a1, a2, a3, a4, a5], s5)

main :: IO ()
main =
  do
    print (monday Locked)

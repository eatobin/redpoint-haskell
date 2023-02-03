module Main (main) where

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

push :: TurnstileState -> (TurnstileOutput, TurnstileState)
push Locked = (Tut, Locked)
push Unlocked = (Open, Locked)

regularPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
regularPerson s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
   in ([a1, a2], s2)

distractedPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
distractedPerson s0 =
  let (a1, s1) = coin s0
   in ([a1], s1)

hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
hastyPerson s0 =
  let (a1, s1) = push s0
   in if a1 == Open
        then ([a1], s1)
        else
          let (a2, s2) = coin s1
              (a3, s3) = push s2
           in ([a1, a2, a3], s3)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
   in ([a1, a2, a3, a4, a5], s5)

tuesday :: TurnstileState -> ([[TurnstileOutput]], TurnstileState)
tuesday s0 =
  let (a1, s1) = regularPerson s0
      (a2, s2) = hastyPerson s1
      (a3, s3) = distractedPerson s2
      (a4, s4) = hastyPerson s3
   in ([a1, a2, a3, a4], s4)

main :: IO ()
main =
  do
    print (monday Locked)
    print (regularPerson Locked)
    print (distractedPerson Locked)
    print (hastyPerson Locked)
    print (hastyPerson Unlocked)
    print (tuesday Locked)

--Implement luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState) representing two people attempting to use the turnstile in succession. The first is either a regularPerson or a distractedPerson (depending on the Bool argument). The second person will simply push the arm without inserting a coin and give up if they don't get through. The Bool result should indicate whether the second person made it through.
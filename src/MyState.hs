{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

--module MyState (RosterName, RosterYear, Quit, MyState (..), myStatePrintResults, myStateSelectNewGiver, myStateGiveeIsSuccess, myStateGiveeIsFailure, myStateUpdateAndRunNewYear, myStateDrawPuck, myStateStartNewYear, myStateAskContinue, myStateErrors, myStateJsonStringToState, myStateMain) where
module MyState (RosterName, RosterYear, Quit, MyState (..), myStateDrawPuck, myStateStartNewYear, myStateGiveeIsFailure, myStateGiveeIsSuccess, myStateSelectNewGiver, myStateErrors, myStatePrintResults, myStateAskContinue, myStateJsonStringToMyState) where

import qualified Control.Monad as CM
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
--import qualified Data.Char as DC
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM
import qualified Data.Set as Set
import qualified GHC.Generics as G
import Gift_History
import Gift_Pair
import Hat
import Players
import Rules
import qualified System.IO as SIO
import qualified System.Random as Ran

--import Text.RawString.QQ (r)

type RosterName = String

type RosterYear = Int

type Quit = String

data MyState = MyState
  { rosterName :: RosterName,
    rosterYear :: RosterYear,
    players :: Players,
    giftYear :: GiftYear,
    giveeHat :: Hat,
    giverHat :: Hat,
    maybeGivee :: Maybe Givee,
    maybeGiver :: Maybe Giver,
    discards :: Discards,
    quit :: Quit
  }
  deriving (Show, Eq, G.Generic)

instance A.FromJSON MyState

myStateDrawPuck :: Hat -> IO (Maybe PlayerKey)
myStateDrawPuck hat
  | Set.null hat = return Nothing
  | otherwise = do
    i :: Int <- Ran.randomRIO (0, Prelude.length hat - 1)
    return (Just (Set.elemAt i hat))

myStateStartNewYear :: IO MyState -> IO MyState
myStateStartNewYear ioState = do
  state <- ioState
  let freshHat :: Hat = hatMakeHat (players state)
   in do
        newGivee <- myStateDrawPuck freshHat
        newGiver <- myStateDrawPuck freshHat
        return
          state
            { rosterName = rosterName state,
              rosterYear = rosterYear state,
              players = playersAddYear (players state),
              giftYear = giftYear state + 1,
              giveeHat = freshHat,
              giverHat = freshHat,
              maybeGivee = newGivee,
              maybeGiver = newGiver,
              discards = Set.empty,
              quit = quit state
            }

myStateGiveeIsFailure :: IO MyState -> IO MyState
myStateGiveeIsFailure ioState = do
  state <- ioState
  let giveeToRemove :: Givee = DM.fromJust (maybeGivee state)
      diminishedGiveeHat :: Hat = hatRemovePuck giveeToRemove (giveeHat state)
   in do
        newGivee <- myStateDrawPuck diminishedGiveeHat
        return
          state
            { rosterName = rosterName state,
              rosterYear = rosterYear state,
              players = players state,
              giftYear = giftYear state,
              giveeHat = diminishedGiveeHat,
              giverHat = giverHat state,
              maybeGivee = newGivee,
              maybeGiver = maybeGiver state,
              discards = hatDiscardGivee giveeToRemove (discards state),
              quit = quit state
            }

myStateGiveeIsSuccess :: IO MyState -> IO MyState
myStateGiveeIsSuccess ioState = do
  state <- ioState
  let currentGiver :: Giver = DM.fromJust (maybeGiver state)
      currentGivee :: Givee = DM.fromJust (maybeGivee state)
      updatedGiveePlayers :: Players = playersUpdateMyGivee currentGiver currentGivee (giftYear state) (players state)
   in do
        return
          state
            { rosterName = rosterName state,
              rosterYear = rosterYear state,
              players = playersUpdateMyGiver currentGivee currentGiver (giftYear state) updatedGiveePlayers,
              giftYear = giftYear state,
              giveeHat = hatRemovePuck currentGivee (giveeHat state),
              giverHat = giverHat state,
              maybeGivee = Nothing,
              maybeGiver = maybeGiver state,
              discards = discards state,
              quit = quit state
            }

myStateSelectNewGiver :: IO MyState -> IO MyState
myStateSelectNewGiver ioState = do
  state <- ioState
  let giverToRemove :: Giver = DM.fromJust (maybeGiver state)
      replenishedGiveeHat :: Hat = hatReturnDiscards (discards state) (giveeHat state)
      diminishedGiverHat :: Hat = hatRemovePuck giverToRemove (giverHat state)
   in do
        newGivee <- myStateDrawPuck replenishedGiveeHat
        newGiver <- myStateDrawPuck diminishedGiverHat
        return
          state
            { rosterName = rosterName state,
              rosterYear = rosterYear state,
              players = players state,
              giftYear = giftYear state,
              giveeHat = replenishedGiveeHat,
              giverHat = diminishedGiverHat,
              maybeGivee = newGivee,
              maybeGiver = newGiver,
              discards = Set.empty,
              quit = quit state
            }

myStateErrors :: IO MyState -> IO [PlayerKey]
myStateErrors ioState = do
  state <- ioState
  let playerKeys :: [PlayerKey] = List.sort (Map.keys (players state))
      playerErrors :: [PlayerKey] =
        [ playerKeyMe
          | playerKeyMe <- playerKeys,
            let myGiverKey = playersGetMyGiver playerKeyMe (players state) (giftYear state),
            let myGiveeKey = playersGetMyGivee playerKeyMe (players state) (giftYear state),
            (playerKeyMe == myGiverKey) || (playerKeyMe == myGiveeKey)
        ]
   in return (List.sort playerErrors)

myStatePrintResults :: IO MyState -> IO MyState
myStatePrintResults ioState = do
  state <- ioState
  errorList <- myStateErrors ioState
  print errorList
  print state
  putStrLn ("\n" ++ rosterName state ++ " - Year " ++ show (rosterYear state + giftYear state) ++ " Gifts:\n")
  let playerKeys :: [PlayerKey] = List.sort (Map.keys (players state))
  mapM_
    putStrLn
    [ do
        let pName = playersGetPlayerName playerKey (players state)
        let giveeKey = playersGetMyGivee playerKey (players state) (giftYear state)
        let giveeName = playersGetPlayerName giveeKey (players state)
        let giverKey = playersGetMyGiver playerKey (players state) (giftYear state)
        if (playerKey == giveeKey) && (playerKey == giverKey)
          then pName ++ " is neither **buying** for nor **receiving** from anyone - **ERROR**"
          else
            if playerKey == giverKey
              then pName ++ " is **receiving** from no one - **ERROR**"
              else
                if playerKey == giveeKey
                  then pName ++ " is **buying** for no one - **ERROR**"
                  else pName ++ " is buying for " ++ giveeName
      | playerKey <- playerKeys
    ]
  CM.unless (null errorList) $ do
    putStrLn "\nThere is a logic error in this year's pairings."
    putStrLn "Do you see how it occurs?"
    putStrLn "If not... call me and I'll explain!\n"
  return state

myStateAskContinue :: IO MyState -> IO MyState
myStateAskContinue ioState = do
  state <- ioState
  putStr "\nContinue? ('q' to quit): "
  SIO.hFlush SIO.stdout
  reply <- getLine
  return state {quit = reply}

myStateJsonStringToMyState :: JsonString -> Maybe MyState
myStateJsonStringToMyState jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe MyState

--myStateUpdateAndRunNewYear :: IO MyState -> IO MyState
--myStateUpdateAndRunNewYear ioState = do
--  myStateUpdateAndRunNewYearLoop (myStateStartNewYear ioState)
--
--myStateLoop :: IO MyState -> IO MyState
--myStateLoop alteredStateIO = do
--  alteredState <- alteredStateIO
--  if DM.isJust (maybeGiver alteredState)
--    then do
--      if DM.isJust (maybeGivee alteredState)
--        then do
--          if rulesGiveeNotSelf (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState))
--            && rulesGiveeNotReciprocal (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState)) (players alteredState) (giftYear alteredState)
--            && rulesGiveeNotRepeat (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState)) (giftYear alteredState) (players alteredState)
--            then myStateLoop (myStateGiveeIsSuccess (return alteredState))
--            else myStateLoop (myStateGiveeIsFailure (return alteredState))
--        else myStateLoop (myStateSelectNewGiver (return alteredState))
--    else return alteredState

myStateLoop :: IO MyState -> IO MyState
myStateLoop alteredStateIO = do
  alteredState <- alteredStateIO
  if DM.isJust (maybeGiver alteredState)
    then
      if DM.isJust (maybeGivee alteredState)
        then
          if rulesGiveeNotSelf (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState)) && rulesGiveeNotReciprocal (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState)) (players alteredState) (giftYear alteredState) && rulesGiveeNotRepeat (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState)) (giftYear alteredState) (players alteredState)
            then myStateLoop (myStateGiveeIsSuccess (return alteredState))
            else myStateLoop (myStateGiveeIsFailure (return alteredState))
        else myStateLoop (myStateSelectNewGiver (return alteredState))
    else return alteredState

--if DM.isJust (maybeGiver alteredState)
--   then
--     if DM.isJust (maybeGivee alteredState)
--        then
--          if rulesGiveeNotSelf (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState)) && rulesGiveeNotReciprocal (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState)) (players alteredState) (giftYear alteredState) && rulesGiveeNotRepeat (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState)) (giftYear alteredState) (players alteredState)
--             then
--               myStateLoop (myStateGiveeIsSuccess (return alteredState))
--             else
--               myStateLoop (myStateGiveeIsFailure (return alteredState))
--        else
--          myStateLoop (myStateSelectNewGiver (return alteredState))
--   else
--     return alteredState

--if foo1
--   then
--     if foo2
--        then
--          if foo3
--             then
--               thing3
--             else
--               otherThing3
--        else
--          otherThing2
--   else
--     otherThing1

--myStateLoop :: IO MyState -> IO ()
--myStateLoop nextIOState = do
--  nextState <- nextIOState
--  if map DC.toLower (quit nextState) == "q"
--    then do
--      putStrLn "\nThis was fun!"
--      putStrLn "Talk about a position with Redpoint?"
--      putStrLn "Please call: Eric Tobin 773-679-6617"
--      putStrLn "Thanks! Bye...\n"
--    else myStateLoop (myStateAskContinue (myStatePrintResults (myStateUpdateAndRunNewYear (return nextState))))
--
--myStateMain :: IO ()
--myStateMain =
--  do
--    let maybeState :: Maybe MyState = myStateJsonStringToState hawksJson
--    case maybeState of
--      Just firstState -> myStateLoop (myStateAskContinue (myStatePrintResults (return firstState)))
--      Nothing -> putStrLn "So sorry, there is an error here."

-- * HatSpec

-- λ> Set.deleteAt 0 hat1
-- fromList ["JohLen","PauMcc","RinSta"]

-- * HatSpec

-- λ> Set.deleteAt 1 hat1
-- fromList ["GeoHar","PauMcc","RinSta"]

-- * HatSpec

-- λ>

--drawInt :: Int -> Int -> IO Int
--drawInt x y = Ran.getStdRandom (Ran.randomR (x,y))
--
--drawDouble :: Double -> Double  -> IO Double
--drawDouble x y = Ran.getStdRandom (Ran.randomR (x,y))

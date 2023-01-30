{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (RosterName, RosterYear, Quit, State (..), mainPrintResults, mainSelectNewGiver, mainGiveeIsSuccess, mainGiveeIsFailure, mainUpdateAndRunNewYear, mainDrawPuck, mainStartNewYear, mainAskContinue, mainErrors, mainJsonStringToState, main) where

import qualified Control.Monad as CM
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as DC
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM
import qualified Data.Set as Set
import qualified Data.List as List
import qualified GHC.Generics as G
import Gift_History
import Gift_Pair
import Hat
import Players
import Rules
import qualified System.IO as SIO
import qualified System.Random as Ran
import Text.RawString.QQ (r)

type RosterName = String

type RosterYear = Int

type Quit = String

data State = State
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

instance A.FromJSON State

mainDrawPuck :: Hat -> IO (Maybe PlayerKey)
mainDrawPuck hat
  | Set.null hat = return Nothing
  | otherwise = do
    i :: Int <- Ran.randomRIO (0, Prelude.length hat - 1)
    return (Just (Set.elemAt i hat))

mainStartNewYear :: IO State -> IO State
mainStartNewYear ioState = do
  state <- ioState
  let freshHat :: Hat = hatMakeHat (players state)
   in do
        newGivee <- mainDrawPuck freshHat
        newGiver <- mainDrawPuck freshHat
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

mainSelectNewGiver :: IO State -> IO State
mainSelectNewGiver ioState = do
  state <- ioState
  let giverToRemove :: Giver = DM.fromJust (maybeGiver state)
      replenishedGiveeHat :: Hat = hatReturnDiscards (discards state) (giveeHat state)
      diminishedGiverHat :: Hat = hatRemovePuck giverToRemove (giverHat state)
   in do
        newGivee <- mainDrawPuck replenishedGiveeHat
        newGiver <- mainDrawPuck diminishedGiverHat
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

mainGiveeIsSuccess :: IO State -> IO State
mainGiveeIsSuccess ioState = do
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

mainGiveeIsFailure :: IO State -> IO State
mainGiveeIsFailure ioState = do
  state <- ioState
  let giveeToRemove :: Givee = DM.fromJust (maybeGivee state)
      diminishedGiveeHat :: Hat = hatRemovePuck giveeToRemove (giveeHat state)
   in do
        newGivee <- mainDrawPuck diminishedGiveeHat
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

mainUpdateAndRunNewYear :: IO State -> IO State
mainUpdateAndRunNewYear ioState = do
  mainUpdateAndRunNewYearLoop (mainStartNewYear ioState)

mainUpdateAndRunNewYearLoop :: IO State -> IO State
mainUpdateAndRunNewYearLoop ioState = do
  alteredState <- ioState
  if DM.isJust (maybeGiver alteredState)
    then do
      if DM.isJust (maybeGivee alteredState)
        then do
          if rulesGiveeNotSelf (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState))
            && rulesGiveeNotReciprocal (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState)) (players alteredState) (giftYear alteredState)
            && rulesGiveeNotRepeat (DM.fromJust (maybeGiver alteredState)) (DM.fromJust (maybeGivee alteredState)) (giftYear alteredState) (players alteredState)
            then mainUpdateAndRunNewYearLoop (mainGiveeIsSuccess (return alteredState))
            else mainUpdateAndRunNewYearLoop (mainGiveeIsFailure (return alteredState))
        else mainUpdateAndRunNewYearLoop (mainSelectNewGiver (return alteredState))
    else return alteredState

mainErrors :: IO State -> IO [PlayerKey]
mainErrors ioState = do
  state <- ioState
  let playerKeys :: [PlayerKey] = List.sort (Map.keys (players state))
      playerErrors :: [PlayerKey] =
        [ playerKeyMe ++ "-" ++ myGiveeKey++ "-" ++ myGiverKey
          | playerKeyMe <- playerKeys,
            let myGiverKey = playersGetMyGiver playerKeyMe (players state) (giftYear state),
            let myGiveeKey = playersGetMyGivee playerKeyMe (players state) (giftYear state)
        ]
   in return (List.sort playerErrors)

mainPrintResults :: IO State -> IO State
mainPrintResults ioState = do
  state <- ioState
  errorList <- mainErrors ioState
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
        else if playerKey == giverKey
          then pName ++ " is **receiving** from no one - **ERROR**"
        else if playerKey == giveeKey
          then pName ++ " is **buying** for no one - **ERROR**"
        else pName ++ " is buying for " ++ giveeName
      | playerKey <- playerKeys
    ]
  CM.unless (null errorList) $ do
    putStrLn "\nThere is a logic error in this year's pairings."
    putStrLn "Do you see how it occurs?"
    putStrLn "If not... call me and I'll explain!\n"
  return state

mainAskContinue :: IO State -> IO State
mainAskContinue ioState = do
  state <- ioState
  putStr "\nContinue? ('q' to quit): "
  SIO.hFlush SIO.stdout
  reply <- getLine
  return state {quit = reply}

mainJsonStringToState :: JsonString -> Maybe State
mainJsonStringToState jsonString = A.decodeStrict (BS.pack jsonString) :: Maybe State

hawksJson :: JsonString
hawksJson = [r|{"rosterName":"Blackhawks","rosterYear":2010,"players":{"TroBro":{"playerName":"Troy Brouwer","giftHistory":[{"givee":"DavBol","giver":"JoeQue"}]},"PatKan":{"playerName":"Patrick Kane","giftHistory":[{"givee":"BryBic","giver":"CriHue"}]},"JoeQue":{"playerName":"Joel Quenneville","giftHistory":[{"givee":"TroBro","giver":"AndLad"}]},"NikHja":{"playerName":"Niklas Hjalmarsson","giftHistory":[{"givee":"BreSea","giver":"BriCam"}]},"TomKop":{"playerName":"Tomas Kopecky","giftHistory":[{"givee":"CriHue","giver":"DunKei"}]},"BryBic":{"playerName":"Bryan Bickell","giftHistory":[{"givee":"MarHos","giver":"PatKan"}]},"AntNie":{"playerName":"Antti Niemi","giftHistory":[{"givee":"JonToe","giver":"MarHos"}]},"PatSha":{"playerName":"Patrick Sharp","giftHistory":[{"givee":"BriCam","giver":"DavBol"}]},"DunKei":{"playerName":"Duncan Keith","giftHistory":[{"givee":"TomKop","giver":"AdaBur"}]},"BriCam":{"playerName":"Brian Campbell","giftHistory":[{"givee":"NikHja","giver":"PatSha"}]},"BreSea":{"playerName":"Brent Seabrook","giftHistory":[{"givee":"KriVer","giver":"NikHja"}]},"KriVer":{"playerName":"Kris Versteeg","giftHistory":[{"givee":"AndLad","giver":"BreSea"}]},"MarHos":{"playerName":"Marian Hossa","giftHistory":[{"givee":"AntNie","giver":"BryBic"}]},"AndLad":{"playerName":"Andrew Ladd","giftHistory":[{"givee":"JoeQue","giver":"KriVer"}]},"DavBol":{"playerName":"Dave Bolland","giftHistory":[{"givee":"PatSha","giver":"TroBro"}]},"CriHue":{"playerName":"Cristobal Huet","giftHistory":[{"givee":"PatKan","giver":"TomKop"}]},"JonToe":{"playerName":"Jonathan Toews","giftHistory":[{"givee":"AdaBur","giver":"AntNie"}]},"AdaBur":{"playerName":"Adam Burish","giftHistory":[{"givee":"DunKei","giver":"JonToe"}]}},"giftYear":0,"giveeHat":[],"giverHat":[],"maybeGivee":null,"maybeGiver":null,"discards":[],"quit":"n"}|]

mainLoop :: IO State -> IO ()
mainLoop nextIOState = do
  nextState <- nextIOState
  if map DC.toLower (quit nextState) == "q"
    then do
      putStrLn "\nThis was fun!"
      putStrLn "Talk about a position with Redpoint?"
      putStrLn "Please call: Eric Tobin 773-679-6617"
      putStrLn "Thanks! Bye...\n"
    else mainLoop (mainAskContinue (mainPrintResults (mainUpdateAndRunNewYear (return nextState))))

main :: IO ()
main =
  do
    let maybeState :: Maybe State = mainJsonStringToState hawksJson
    case maybeState of
      Just firstState -> mainLoop (mainAskContinue (mainPrintResults (return firstState)))
      Nothing -> putStrLn "So sorry, there is an error here."

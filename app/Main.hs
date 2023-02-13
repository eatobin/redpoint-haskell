{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import qualified Data.Char as DC
import qualified Data.Maybe as DM
import Gift_Pair
import MyState
import Text.RawString.QQ (r)

hawksJson :: JsonString
hawksJson = [r|{"rosterName":"Blackhawks","rosterYear":2010,"players":{"TroBro":{"playerName":"Troy Brouwer","giftHistory":[{"givee":"DavBol","giver":"JoeQue"}]},"PatKan":{"playerName":"Patrick Kane","giftHistory":[{"givee":"BryBic","giver":"CriHue"}]},"JoeQue":{"playerName":"Joel Quenneville","giftHistory":[{"givee":"TroBro","giver":"AndLad"}]},"NikHja":{"playerName":"Niklas Hjalmarsson","giftHistory":[{"givee":"BreSea","giver":"BriCam"}]},"TomKop":{"playerName":"Tomas Kopecky","giftHistory":[{"givee":"CriHue","giver":"DunKei"}]},"BryBic":{"playerName":"Bryan Bickell","giftHistory":[{"givee":"MarHos","giver":"PatKan"}]},"AntNie":{"playerName":"Antti Niemi","giftHistory":[{"givee":"JonToe","giver":"MarHos"}]},"PatSha":{"playerName":"Patrick Sharp","giftHistory":[{"givee":"BriCam","giver":"DavBol"}]},"DunKei":{"playerName":"Duncan Keith","giftHistory":[{"givee":"TomKop","giver":"AdaBur"}]},"BriCam":{"playerName":"Brian Campbell","giftHistory":[{"givee":"NikHja","giver":"PatSha"}]},"BreSea":{"playerName":"Brent Seabrook","giftHistory":[{"givee":"KriVer","giver":"NikHja"}]},"KriVer":{"playerName":"Kris Versteeg","giftHistory":[{"givee":"AndLad","giver":"BreSea"}]},"MarHos":{"playerName":"Marian Hossa","giftHistory":[{"givee":"AntNie","giver":"BryBic"}]},"AndLad":{"playerName":"Andrew Ladd","giftHistory":[{"givee":"JoeQue","giver":"KriVer"}]},"DavBol":{"playerName":"Dave Bolland","giftHistory":[{"givee":"PatSha","giver":"TroBro"}]},"CriHue":{"playerName":"Cristobal Huet","giftHistory":[{"givee":"PatKan","giver":"TomKop"}]},"JonToe":{"playerName":"Jonathan Toews","giftHistory":[{"givee":"AdaBur","giver":"AntNie"}]},"AdaBur":{"playerName":"Adam Burish","giftHistory":[{"givee":"DunKei","giver":"JonToe"}]}},"giftYear":0,"giveeHat":[],"giverHat":[],"maybeGivee":null,"maybeGiver":null,"discards":[],"quit":"n"}|]

myMainLoop :: MyState -> IO ()
myMainLoop nextState = do
  if map DC.toLower (quit nextState) == "q"
    then do
      putStrLn "\nThis was fun!"
      putStrLn "Talk about a position with Redpoint?"
      putStrLn "Please call: Eric Tobin 773-679-6617"
      putStrLn "Thanks! Bye...\n"
    else
      myStateUpdateAndRunNewYear nextState
        >>= myStatePrintResults
        >>= myStateAskContinue
        >>= myMainLoop

main :: IO ()
main =
  do
    let firstStateIO = return (DM.fromJust (myStateJsonStringToMyState hawksJson))
    firstStateIO
      >>= myStatePrintResults
      >>= myStateAskContinue
      >>= myMainLoop

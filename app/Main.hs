{-# LANGUAGE ExistentialQuantification, ExplicitNamespaces, FlexibleContexts, FlexibleInstances, KindSignatures, LiberalTypeSynonyms, MultiParamTypeClasses, PostfixOperators, RankNTypes, ScopedTypeVariables, UnicodeSyntax, UnliftedFFITypes #-}

module Main where

import Data.List.Split ( splitOn )
import System.Exit ( exitSuccess )
import Util.Fen
    ( startPosition,
      algebraicMoveFromMove,
      moveFromAlgebraicMove,
      getPosition,
      verifyFen )
import Types
import Search.MakeMove ( makeMove )
import Alias ()
import Search.Search ( startSearch )
import Text.Printf ( printf )
import Util.Utils ( timeMillis )
import System.IO ( stdout, hFlush )
import Data.IORef ()
import State.State ( SearchState, makeSearchState, showStats, zeroStats, showPv, startStats, stats, setMillisTaken )
import qualified Data.HashTable.IO as H

data UCIState = UCIState {
      position :: [Position]
    , quit :: Bool
    , errorMessage :: String
    , output :: String
    , searchState :: SearchState
}

main :: IO ()
main = do
    h <- H.new
    searchState <- makeSearchState h startStats [] 0
    commandCycle UCIState {position = [getPosition startPosition], quit=False, errorMessage="", output="", searchState=searchState}

showId :: IO ()
showId = do
    putStrLn "id name Rival Haskell Build 1303"
    putStrLn "id author Chris Moreton"
    putStrLn "uciok"

commandCycle :: UCIState -> IO ()
commandCycle uciState = do
  hFlush stdout
  command <- getLine
  uciState' <- run uciState (splitOn " " command)
  let e = errorMessage uciState'
  let o = output uciState'
  let c = searchState uciState
  showStats c
  zeroStats c
  if quit uciState'
      then do
          putStrLn "Bye"
          return ()
      else do
          if e == ""
             then if o == ""
                    then commandCycle uciState'
                    else do
                        putStrLn (output uciState')
                        commandCycle uciState'{errorMessage="",output=""}
             else do
                putStrLn e
                commandCycle uciState'{errorMessage="",output=""}

run :: UCIState -> [String] -> IO UCIState
run uciState ("uci":xs) = do
    showId
    return uciState

run uciState ("go":xs) = runGo uciState xs

run uciState ("isready":xs) = do
    putStrLn "readyok"
    return uciState

run uciState ("position":xs) = runPosition uciState xs

run uciState ("quit":_) = return uciState{quit=True}

run uciState ("test1":_) = runPosition uciState ["fen","r5rk/5p1p/5R2/4B3/8/8/7P/7K w - - 1 1"]
run uciState ("test2":_) = runPosition uciState ["fen","6k1/3b3r/1p1p4/p1n2p2/1PPNpP1q/P3Q1p1/1R1RB1P1/5K2 b - - 0 1"]

run uciState (x:xs) = return uciState

runGo :: UCIState -> [String] -> IO UCIState
runGo uciState ("infinite":_) = runGo uciState ["movetime","10000000"]

runGo uciState (command:xs) = do
    t1 <- timeMillis
    let param = read (head xs)
    let endTime = t1 + if command == "depth" then 1000000 else param
    let depth
          | command == "movetime" = 50
          | param > 0 = param
          | otherwise = 1
    move <- startSearch (position uciState) depth endTime (searchState uciState)
    t2 <- timeMillis
    setMillisTaken (t2 - t1) (searchState uciState)
    pvText <- showPv (searchState uciState) (head (position uciState)) ""
    return uciState {
        output = "info score cp " ++ show (msScore move) ++ " pv" ++ pvText ++ "\n" ++
                 "bestmove " ++ algebraicMoveFromMove (head $ msPath move)
    }

runPosition :: UCIState -> [String] -> IO UCIState
runPosition uciState ("startpos":xs) = runPosition uciState (["fen",startPosition] ++ xs)

runPosition uciState ("fen":xs) = do
    let parts = splitOn " moves " $ stringArrayToWords xs
    let fen = head parts
    let error = verifyFen fen
    let moveList = if length parts > 1 then splitOn " " (parts !! 1) else []
    if error == ""
        then do
            if not (null moveList)
                then return (updatePosition uciState{position=[getPosition fen]} moveList)
                else return uciState{position=[getPosition fen]}
        else
            return uciState{errorMessage=error}

updatePosition :: UCIState -> [String] -> UCIState
updatePosition uciState [] = uciState
updatePosition uciState moveList =
    updatePosition uciState{position=makeMove (head (position uciState)) (moveFromAlgebraicMove(head moveList)) : position uciState, errorMessage=""} (tail moveList)

stringArrayToWords :: [String] -> String
stringArrayToWords (x:xs) = x ++ concatMap (" " ++) xs
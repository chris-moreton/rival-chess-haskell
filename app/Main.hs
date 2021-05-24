{-# LANGUAGE TupleSections #-}

module Main where

import Data.List.Split ( splitOn )
import System.Exit ( exitSuccess )
import Util.Fen
    ( startPosition,
      algebraicMoveFromMove,
      moveFromAlgebraicMove,
      getPosition,
      verifyFen )
import Types ( Position )
import Search.MakeMove ( makeMove )
import Alias ()
import Search.Search ( startSearch )
import Text.Printf ( printf )
import Util.Utils ( timeMillis )
import System.IO ( stdout, hFlush )

data UCIState = UCIState {
      position :: [Position]
    , quit :: Bool
    , errorMessage :: String
    , output :: String
}

main :: IO ()
main = do
    commandCycle UCIState {position = [getPosition startPosition], quit=False, errorMessage="", output=""}

showId :: IO ()
showId = do
    putStrLn "id name Rival Haskell Build 30"
    putStrLn "id author Chris Moreton"
    putStrLn "uciok"

commandCycle :: UCIState -> IO ()
commandCycle uciState = do
  hFlush stdout
  command <- getLine
  uciState' <- run uciState (splitOn " " command)
  let e = errorMessage uciState'
  let o = output uciState'
  if quit uciState'
      then do
          putStrLn "Bye"
          exitSuccess
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

run uciState (x:xs) = do
    return uciState

runGo :: UCIState -> [String] -> IO UCIState
runGo uciState ("infinite":_) = runGo uciState ["movetime","10000000"]

runGo uciState ("movetime":xs) = do
    let moveTime = head xs
    t <- timeMillis
    let endTime = t + (read moveTime)
    move <- startSearch (position uciState) 50 endTime
    return uciState{output="bestmove " ++ algebraicMoveFromMove (fst move)}

runGo uciState ("depth":xs) = do
    let depth = read (head xs)
    t <- timeMillis
    let endTime = t + 1000000
    move <- startSearch (position uciState) depth endTime
    return uciState{output="bestmove " ++ algebraicMoveFromMove (fst move)}

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
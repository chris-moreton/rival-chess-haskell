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
import Search.Search ( search )
import Text.Printf ( printf )
import Util.Utils
import System.IO

data UCIState = UCIState {
      position :: Position
    , quit :: Bool
    , errorMessage :: String
    , output :: String
}

main :: IO ()
main = do
    commandCycle UCIState {position = getPosition startPosition, quit=False, errorMessage="", output=""}

showId :: IO ()
showId = do
    putStrLn "id name Rival Haskell"
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
    let endTime = t + read moveTime
    move <- search (position uciState) endTime
    return uciState{output="bestmove " ++ (algebraicMoveFromMove move)}

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
                then return uciState{position=makeAlgebraicMoves (getPosition fen) moveList, errorMessage=""}
                else return uciState{position=getPosition fen}
        else
            return uciState{errorMessage=error}

makeAlgebraicMoves :: Position -> [String] -> Position
makeAlgebraicMoves = foldl (\ position move -> makeMove position (moveFromAlgebraicMove move))

stringArrayToWords :: [String] -> String
stringArrayToWords (x:xs) = x ++ concatMap (" " ++) xs
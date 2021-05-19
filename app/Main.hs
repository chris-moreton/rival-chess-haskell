module Main where

import Data.List.Split ( splitOn )
import System.Exit
import Util.Fen
import Types
import Search.MakeMove
import Alias

data UCIState = UCIState {
      position :: Position
    , quit :: Bool
    , errorMessage :: String
}

main :: IO ()
main = do
  let uciState = UCIState {position = getPosition startPosition, quit=False, errorMessage=""}
  putStrLn "Hello"
  command <- getLine
  uciState' <- run uciState (splitOn " " command)
  let e = errorMessage uciState'
  putStrLn e
  if (quit uciState') 
      then do
          putStrLn "Bye"
          exitSuccess
      else do
          print ("EnPassant Square is " ++ (show (enPassantSquare (position uciState'))))
          main     

run :: UCIState -> [String] -> IO UCIState
run uciState ("uci":xs) = do 
    putStrLn "id name Rival Haskell"
    putStrLn "id author Chris Moreton"
    return uciState

run uciState ("position":xs) = runPosition uciState xs

run uciState ("quit":_) = do 
    return uciState{quit=True}

run uciState (x:xs) = do
    putStrLn x
    return uciState

runPosition :: UCIState -> [String] -> IO UCIState
runPosition uciState ("startpos":xs) = runPosition uciState (["fen",startPosition] ++ xs)

runPosition uciState ("fen":xs) = do
    let parts = splitOn " moves " $ stringArrayToWords xs
    let fen = head parts
    print $ show fen
    let moves = splitOn " " (head (tail parts))
    print $ show moves
    return uciState{position=makeMoves (getPosition fen) moves}

makeMoves :: Position -> [String] -> Position
makeMoves position [] = position
makeMoves position (move:moves) = makeMoves (makeMove position (moveFromAlgebraicMove move)) moves

stringArrayToWords :: [String] -> String
stringArrayToWords (x:xs) = x ++ foldr (++) "" (map (\x -> " " ++ x) xs)

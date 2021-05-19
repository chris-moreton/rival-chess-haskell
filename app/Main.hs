module Main where

import Data.List.Split ( splitOn )
import System.Exit

data UCIState = UCIState {
      position :: String
    , quit :: Bool
    , errorMessage :: String
}

main :: IO ()
main = do
  let uciState = UCIState {position = "", quit=False, errorMessage=""}
  putStrLn "Hello"
  command <- getLine
  uciState' <- run uciState (splitOn " " command)
  let e = errorMessage uciState'
  putStrLn e
  if (quit uciState') 
      then do
          putStrLn "Bye"
          exitSuccess
      else if (e /= "")
            then putStrLn e
            else main
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
runPosition uciState ("startpos":_) = do
    return uciState

runPosition uciState ("fen":xs) = do
    if length xs /= 6
        then return uciState{errorMessage="Invalid FEN: " ++ stringArrayToWords xs}
        else return uciState

stringArrayToWords :: [String] -> String
stringArrayToWords (x:xs) = x ++ foldr (++) "" (map (\x -> " " ++ x) xs)

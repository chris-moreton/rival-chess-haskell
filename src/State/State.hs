module State.State where

import Data.IORef ( modifyIORef, writeIORef, newIORef, readIORef, IORef )
import qualified Data.HashTable.IO as H
import Types ( HashTable, HashEntry, Position )
import Alias ( Move )
import Util.Fen ( algebraicMoveFromMove )

data SearchState = SearchState {
     hashTable  :: HashTable
   , nodes      :: IORef Integer
   , pv         :: [IORef Move]
   , pvScore    :: IORef Integer
}

makeSearchState :: HashTable -> Integer -> [IORef Move] -> Integer -> IO SearchState
makeSearchState h n pv s = do
    hRef <- H.new
    nRef <- newIORef n
    sRef <- newIORef s
    return (SearchState hRef nRef pv sRef)

incNodes :: Integer -> SearchState -> IO ()
incNodes i (SearchState _ nodes _ _) = do modifyIORef nodes (+ i)

setPvMove :: Int -> Move -> SearchState -> IO ()
setPvMove depth move (SearchState _ _ moves _) = do
    writeIORef (moves !! depth) move

zeroNodes :: SearchState -> IO ()
zeroNodes (SearchState _ nodes _ _) = do modifyIORef nodes (0 *)

showNodes:: SearchState -> IO ()
showNodes (SearchState _ nodes _ _) = do
    nodes' <- readIORef nodes
    print nodes'

clearPv :: SearchState -> IO SearchState
clearPv searchState = go searchState 100
    where
        go :: SearchState -> Integer -> IO SearchState
        go searchState 0 = return searchState
        go searchState c = do
            r <- newIORef 0
            go (searchState { pv = r : pv searchState }) (c-1)

showPv:: SearchState -> Position -> String -> IO String
showPv (SearchState _ _ [] _) _ result = return ""
showPv searchState position result = do
    let pv' = pv searchState
    move <- readIORef (head pv')
    if move == 0 then do
        return result
    else do
        showPv searchState { pv = tail pv' } position (algebraicMoveFromMove move ++ " " ++ result)

calcHashIndex :: Int -> Int
calcHashIndex i = i `mod` 16777216

updateHashTable :: Int -> HashEntry -> SearchState -> IO ()
updateHashTable i he (SearchState hashTable _ _ _) = do H.insert hashTable (calcHashIndex i) he


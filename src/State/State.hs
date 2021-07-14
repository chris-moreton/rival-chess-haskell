module State.State where

import Data.IORef ( modifyIORef, writeIORef, newIORef, readIORef, IORef )
import qualified Data.HashTable.IO as H
import Types ( HashTable, HashEntry, Position )
import Alias ( Move, Path )
import Util.Fen ( algebraicMoveFromMove )

data SearchState = SearchState {
     hashTable  :: HashTable
   , nodes      :: IORef Integer
   , pv         :: IORef [Move]
   , pvScore    :: IORef Integer
}

makeSearchState :: HashTable -> Integer -> [Move] -> Integer -> IO SearchState
makeSearchState h n p s = do
    hRef <- H.new
    nRef <- newIORef n
    pRef <- newIORef p
    sRef <- newIORef s
    return (SearchState hRef nRef pRef sRef)

incNodes :: Integer -> SearchState -> IO ()
incNodes i (SearchState _ nodes _ _) = do modifyIORef nodes (+ i)

setPv :: Path -> SearchState -> IO ()
setPv pv (SearchState _ _ moves _) = do writeIORef moves pv

zeroNodes :: SearchState -> IO ()
zeroNodes (SearchState _ nodes _ _) = do modifyIORef nodes (0 *)

showNodes:: SearchState -> IO ()
showNodes (SearchState _ nodes _ _) = do
    nodes' <- readIORef nodes
    print nodes'

showPv:: SearchState -> Position -> String -> IO String
showPv (SearchState _ _ pv _) position result = do
    pv' <- readIORef pv
    return (go pv' position "")
    where 
        go :: [Move] -> Position -> String -> String
        go [] _ result = result
        go (m:ms) position result =
            if m == 0 
                then result
                else go ms position (algebraicMoveFromMove m ++ " " ++ result)

calcHashIndex :: Int -> Int
calcHashIndex i = i `mod` 16777216

updateHashTable :: Int -> HashEntry -> SearchState -> IO ()
updateHashTable i he (SearchState hashTable _ _ _) = do H.insert hashTable (calcHashIndex i) he


module State.State where

import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )
import qualified Data.HashTable.IO as H
import Types ( HashTable, HashEntry )
import Alias

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

zeroNodes :: SearchState -> IO ()
zeroNodes (SearchState _ nodes _ _) = do modifyIORef nodes (0 *)

showNodes:: SearchState -> IO ()
showNodes (SearchState _ nodes _ _) = do 
    nodes' <- readIORef nodes
    print nodes'

calcHashIndex :: Int -> Int 
calcHashIndex i = i `mod` 16777216

updateHashTable :: Int -> HashEntry -> SearchState -> IO ()
updateHashTable i he (SearchState hashTable _ _ _) = do H.insert hashTable (calcHashIndex i) he


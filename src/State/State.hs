module State.State where

import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )
import qualified Data.HashTable.IO as H
import Types ( HashTable, HashEntry )

data SearchState = SearchState { 
     hashTable  :: HashTable
   , nodes      :: IORef Integer
}

makeSearchState :: HashTable -> Integer -> IO SearchState
makeSearchState h i = do 
    iref <- newIORef i
    href <- H.new
    return (SearchState href iref)

incNodes :: Integer -> SearchState -> IO ()
incNodes i (SearchState _ c) = do modifyIORef c (+ i)

zeroNodes :: SearchState -> IO ()
zeroNodes (SearchState _ c) = do modifyIORef c (0 *)

showNodes:: SearchState -> IO ()
showNodes (SearchState _ nodes) = do 
    nodes' <- readIORef nodes
    print nodes'

calcHashIndex :: Int -> Int 
calcHashIndex i = i `mod` 16777216

updateHashTable :: Int -> HashEntry -> SearchState -> IO ()
updateHashTable i he (SearchState hashTable _) = do H.insert hashTable (calcHashIndex i) he


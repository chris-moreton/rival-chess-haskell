module State.State where

import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )
import qualified Data.HashTable.IO as H
import Types ( HashTable, HashEntry )

data Counter = Counter { 
     h :: HashTable
   , x :: IORef Int 
}

makeCounter :: HashTable -> Int -> IO Counter
makeCounter h i = do 
    iref <- newIORef i
    href <- H.new
    return (Counter href iref)

incCounter :: Int -> Counter -> IO ()
incCounter i (Counter _ c) = do modifyIORef c (+ i)

decCounter :: Int -> Counter -> IO ()
decCounter i (Counter _ c) = do modifyIORef c (i -)

showCounter :: Counter -> IO ()
showCounter (Counter _ c) = do 
    c' <- readIORef c
    print c'

updateHashTable :: Int -> HashEntry -> Counter -> IO ()
updateHashTable i he (Counter h _) = do H.insert h i he


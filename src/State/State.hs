module State.State where

import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )
import qualified Data.HashTable.IO as H
import Types ( HashTable, HashEntry )

data SearchState = SearchState { 
     h :: HashTable
   , x :: IORef Integer
}

makeCounter :: HashTable -> Integer -> IO SearchState
makeCounter h i = do 
    iref <- newIORef i
    href <- H.new
    return (SearchState href iref)

incCounter :: Integer -> SearchState -> IO ()
incCounter i (SearchState _ c) = do modifyIORef c (+ i)

decCounter :: Integer -> SearchState -> IO ()
decCounter i (SearchState _ c) = do modifyIORef c (i -)

zeroCounter :: SearchState -> IO ()
zeroCounter (SearchState _ c) = do modifyIORef c (0 *)

showCounter :: SearchState -> IO ()
showCounter (SearchState _ c) = do 
    c' <- readIORef c
    print c'

updateHashTable :: Int -> HashEntry -> SearchState -> IO ()
updateHashTable i he (SearchState h _) = do H.insert h i he


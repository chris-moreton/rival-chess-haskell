{-# LANGUAGE KindSignatures,DataKinds #-}

module State.State where

import qualified Data.Vector.Unboxed as V
import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )

newtype Counter = Counter { x :: IORef Int }

data HashEntry = HashEntry { score :: Int, lock :: Int }

data HashTable = HashTable {
    he :: [HashEntry]
}

data SearchState = SearchState {
          counter   :: IORef Int
        , hashTable :: IORef HashTable
    }

incCounter :: Int -> SearchState -> IO ()
incCounter i (SearchState c _) = do modifyIORef c (+ i)

showCounter :: SearchState -> IO ()
showCounter (SearchState c _) = do { c' <- readIORef c; print c' }

makeHashTable :: Int -> HashTable -> IO SearchState
makeHashTable c i = do
    iref <- newIORef i
    cref <- newIORef c
    return (SearchState cref iref)

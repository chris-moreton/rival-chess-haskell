{-# LANGUAGE KindSignatures,DataKinds #-}

module State.State where

import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )
import qualified Data.Vector.Mutable as VM

data HashFlag = Lower | Upper | Exact | None deriving (Show, Eq)
data HashEntry = HashEntry { score :: Int, depth:: Int, flag :: HashFlag, lock :: Int } deriving (Show, Eq)

emptyHashEntry :: HashEntry
emptyHashEntry = HashEntry 0 0 None 0

newtype HashTable = HashTable {
    he :: VM.MVector HashEntry 4096
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

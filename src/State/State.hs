{-# LANGUAGE KindSignatures,DataKinds #-}

module State.State where

import qualified Data.Vector.Storable as V
import GHC.TypeNats ( Nat )
import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )

newtype Counter = Counter { x :: IORef Int }

data HashEntry = HashEntry { score :: Int, lock :: Int }

newtype HashTable (n :: Nat) a = UnsafeMkVec { getVector :: IORef (V.Vector a) }
newtype SearchState = SearchState { h :: IORef (HashTable 4096 HashEntry) }

makeCounter :: Int -> IO Counter
makeCounter i = do iref <- newIORef i
                   return (Counter iref)

incCounter :: Int -> Counter -> IO ()
incCounter i (Counter c) = do modifyIORef c (+ i)

decCounter :: Int -> Counter -> IO ()
decCounter i (Counter c) = do modifyIORef c (i -)

showCounter :: Counter -> IO ()
showCounter (Counter c) = do c' <- readIORef c
                             print c'

makeHashTable :: HashTable 4096 HashEntry -> IO SearchState
makeHashTable i = do
    iref <- newIORef i
    return (SearchState iref)

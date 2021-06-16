module State.State where

import Data.IORef ( modifyIORef, newIORef, readIORef, IORef )

data Counter = Counter { 
     h :: IORef Int
   , x :: IORef Int 
}

makeCounter :: Int -> Int -> IO Counter
makeCounter i j = do 
    iref <- newIORef i
    jref <- newIORef j
    return (Counter iref jref)

incCounter :: Int -> Counter -> IO ()
incCounter i (Counter _ c) = do modifyIORef c (+ i)

decCounter :: Int -> Counter -> IO ()
decCounter i (Counter _ c) = do modifyIORef c (i -)

showCounter :: Counter -> IO ()
showCounter (Counter _ c) = do 
    c' <- readIORef c
    print c'
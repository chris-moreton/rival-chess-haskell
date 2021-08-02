module State.State where

import Data.IORef ( modifyIORef, writeIORef, newIORef, readIORef, IORef )
import qualified Data.HashTable.IO as H
import Types ( HashTable, HashEntry, Position )
import Alias ( Move, Path )
import Util.Fen ( algebraicMoveFromMove )

data Stats = Stats {
      nodes :: Integer
    , hashHitsExact   :: Integer
    , hashHitsLower   :: Integer
    , hashHitsUpper   :: Integer
    , hashHitsQuiesce :: Integer
    , millisTaken     :: Int
}

data SearchState = SearchState {
     hashTable  :: HashTable
   , stats      :: IORef Stats
   , pv         :: IORef [Move]
   , pvScore    :: IORef Integer
}

makeSearchState :: Stats -> [Move] -> Integer -> IO SearchState
makeSearchState n p s = do
    hRef <- H.new
    nRef <- newIORef n
    pRef <- newIORef p
    sRef <- newIORef s
    return (SearchState hRef nRef pRef sRef)

incNodes :: SearchState -> IO ()
incNodes (SearchState _ stats _ _) = do modifyIORef stats go
    where
        go :: Stats -> Stats
        go stats = stats { nodes = nodes stats + 1 }

incHashQuiesce :: SearchState -> IO ()
incHashQuiesce (SearchState _ stats _ _) = do modifyIORef stats go
    where
        go :: Stats -> Stats
        go stats = stats { hashHitsQuiesce = hashHitsQuiesce stats + 1 }

incHashExact :: SearchState -> IO ()
incHashExact (SearchState _ stats _ _) = do modifyIORef stats go
    where
        go :: Stats -> Stats
        go stats = stats { hashHitsExact = hashHitsExact stats + 1 }

incHashLower :: SearchState -> IO ()
incHashLower (SearchState _ stats _ _) = do modifyIORef stats go
    where
        go :: Stats -> Stats
        go stats = stats { hashHitsLower = hashHitsLower stats + 1 }

incHashUpper :: SearchState -> IO ()
incHashUpper (SearchState _ stats _ _) = do modifyIORef stats go
    where
        go :: Stats -> Stats
        go stats = stats { hashHitsUpper = hashHitsUpper stats + 1 }

setMillisTaken :: Int -> SearchState -> IO ()
setMillisTaken mt (SearchState _ stats _ _) = do modifyIORef stats go
    where
        go :: Stats -> Stats
        go stats = stats { millisTaken = mt }

startStats :: Stats
startStats = Stats 0 0 0 0 0 0

setPv :: Path -> SearchState -> IO ()
setPv pv (SearchState _ _ moves _) = do writeIORef moves pv

zeroStats :: SearchState -> IO ()
zeroStats (SearchState _ nodes _ _) = do modifyIORef nodes go
    where
        go :: Stats -> Stats
        go stats = Stats 0 0 0 0 0 0

showStats :: SearchState -> IO ()
showStats (SearchState _ stats _ _) = do
    stats' <- readIORef stats
    let n = nodes stats'
    let mt = millisTaken stats'
    let he = hashHitsExact   stats'
    let hl = hashHitsLower   stats'
    let hu = hashHitsUpper   stats'
    let hq = hashHitsQuiesce stats'
    let nps = (fromIntegral n / fromIntegral mt) * 1000
    putStrLn $ "Nodes = " ++ show n
    putStrLn $ "NPS = " ++ show nps
    putStrLn $ "Hash Hits Exact = " ++ show he
    putStrLn $ "Hash Hits Lower = " ++ show hl
    putStrLn $ "Hash Hits Upper = " ++ show hu
    putStrLn $ "Hash Hits Quiesce = " ++ show hq

showPv:: SearchState -> Position -> String -> IO String
showPv (SearchState _ _ pv _) position result = do
    pv' <- readIORef pv
    return (pathString pv' position "")

pathString :: [Move] -> Position -> String -> String
pathString [] _ result = result
pathString (m:ms) position result = pathString ms position (result ++ " " ++ algebraicMoveFromMove m)

calcHashIndex :: Int -> Int
calcHashIndex i = i `mod` 16777216

updateHashTable :: Int -> HashEntry -> SearchState -> IO ()
updateHashTable i he (SearchState hashTable _ _ _) = do H.insert hashTable (calcHashIndex i) he


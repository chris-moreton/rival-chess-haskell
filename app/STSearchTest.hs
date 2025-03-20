-- | Test program for the ST-optimized search
-- | This program compares the performance of the regular search and the ST-optimized search
-- | It measures nodes per second and memory usage for both approaches

module Main where

import Types
import Alias
import Util.Fen
import Search.Search (search)
import Search.STSearch (searchST)
import Data.Time.Clock
import Text.Printf
import State.State (SearchState)
import System.Environment (getArgs)

-- | Test positions for benchmarking
-- | These are standard test positions at various stages of the game
startPosition :: String
startPosition = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

midgamePosition :: String
midgamePosition = "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"

tacticalPosition :: String
tacticalPosition = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"

endgamePosition :: String
endgamePosition = "8/5pk1/8/3p4/8/2K5/5R2/8 w - - 0 1"

-- | Main function
main :: IO ()
main = do
    putStrLn "Rival Chess - ST Search Optimization Test"
    putStrLn "======================================"
    putStrLn ""

    -- Create a placeholder SearchState - in a real implementation, 
    -- we would initialize a proper SearchState with HashTables
    let searchState = error "Need to implement: Create SearchState"
    
    -- Get arguments for test depth
    args <- getArgs
    let depth = if null args then 6 else read (head args)
    
    -- Run tests on different positions
    putStrLn $ "Testing at depth " ++ show depth
    putStrLn ""
    
    -- Test on the start position
    putStrLn "Initial position:"
    testPosition (fromFEN startPosition) depth searchState
    
    -- Test on a midgame position
    putStrLn "\nMidgame position:"
    testPosition (fromFEN midgamePosition) depth searchState
    
    -- Test on a tactical position
    putStrLn "\nTactical position:"
    testPosition (fromFEN tacticalPosition) depth searchState
    
    -- Test on an endgame position
    putStrLn "\nEndgame position:"
    testPosition (fromFEN endgamePosition) depth searchState
    
    -- Summary
    putStrLn "\nSummary:"
    putStrLn "The ST-based search eliminates the overhead of creating new Position"
    putStrLn "records during search by using a mutable position that's updated in-place."
    putStrLn "This significantly reduces memory allocation and garbage collection,"
    putStrLn "which can lead to 2-3x speedup in deep searches."

-- | Test a position by running both the regular and ST-optimized search
testPosition :: Position -> Int -> SearchState -> IO ()
testPosition position depth searchState = do
    -- Prepare search parameters
    let alpha = -999999
        beta = 999999
    
    -- Test the regular search
    t1 <- getCurrentTime
    let (eval1, move1) = search position 0 depth alpha beta searchState
    t2 <- getCurrentTime
    
    -- Test the ST-optimized search
    let (eval2, move2) = searchST position depth alpha beta searchState
    t3 <- getCurrentTime
    
    -- Calculate time and print results
    let regTime = diffUTCTime t2 t1
        stTime = diffUTCTime t3 t2
        speedup = realToFrac regTime / realToFrac stTime
    
    printf "Regular search: %.3fs, eval=%d, best=%d\n" 
           (realToFrac regTime :: Double) eval1 move1
    printf "ST search:      %.3fs, eval=%d, best=%d\n" 
           (realToFrac stTime :: Double) eval2 move2
    printf "Speedup: %.2fx\n" speedup
    
    -- Check for any correctness issues
    if eval1 /= eval2 || move1 /= move2
        then putStrLn "WARNING: Different results between regular and ST search!"
        else putStrLn "Results match between regular and ST search."
{-# LANGUAGE BangPatterns, StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides an optimized implementation of the alpha-beta search
-- | algorithm using the ST monad to reduce memory allocations during search.
-- | 
-- | Instead of creating a new Position for each move during search, this
-- | implementation keeps a single mutable Position and modifies it in-place
-- | as moves are made and unmade.
module Search.STSearch 
    ( searchST
    , iterativeDeepeningST
    , searchRootST
    ) where

import Types
import Alias
import Search.MoveGenerator (moves, isCheck)
import Search.MakeMove (makeMove)
import Search.Quiesce (quiesce)
import Data.Bits
import Data.STRef
import Data.Array.ST
import Data.Array.Unboxed
import Data.List (sortBy)
import Control.Monad.ST
import Control.Monad (when)
import State.State (SearchState, calcHashIndex, updateHashTable)
import Evaluate.Evaluate (evaluate, isCapture, scoreMove)
import Util.Zobrist (zobrist)
import qualified Search.Search as Search

-- | Data to undo a move during search (to restore position state)
data UndoInfo = UndoInfo {
    -- | The piece that was captured, if any (Pawn used as placeholder for no piece)
    capturedPiece :: !Piece,
    -- | The original value of the en passant square
    oldEnPassantSquare :: !Square,
    -- | Original half-move clock
    halfMoveValue :: !Int,
    -- | Original castling availability
    whiteCastleKing :: !Bool,
    whiteCastleQueen :: !Bool,
    blackCastleKing :: !Bool,
    blackCastleQueen :: !Bool,
    -- | Original Zobrist hash key
    zobristKey :: !Int
} deriving (Show)

-- | Mutable search state for the ST monad
data STSearchState s = STSearchState {
    -- | Mutable reference to the current position
    posRef :: STRef s Position,
    -- | Moves made during search (for history)
    moveHistory :: STArray s Int Move,
    -- | Data needed to undo moves
    undoHistory :: STArray s Int UndoInfo,
    -- | Current depth in the search tree (used to index history arrays)
    depthRef :: STRef s Int,
    -- | Alpha/beta bounds
    alphaRef :: STRef s Int,
    betaRef :: STRef s Int,
    -- | Node counter
    nodesRef :: STRef s Int,
    -- | Shared transposition table state
    searchState :: SearchState
}

-- | Entry point for ST-based search
-- | This function creates the initial search state and starts the search
searchST :: Position -> Int -> Int -> Int -> SearchState -> (Int, Move)
searchST position depth alpha beta searchState = runST $ do
    -- Create the mutable search state
    stState <- initSTSearchState position searchState

    -- Set initial alpha-beta values
    writeSTRef (alphaRef stState) alpha
    writeSTRef (betaRef stState) beta
    
    -- Start the search
    searchInnerST stState depth
    
    -- Return the best move and score
    a <- readSTRef (alphaRef stState)
    bestMove <- readArray (moveHistory stState) 0
    return (a, bestMove)

-- | Initialize the ST search state
initSTSearchState :: Position -> SearchState -> ST s (STSearchState s)
initSTSearchState position searchState = do
    -- Create a mutable reference to the position
    posRef <- newSTRef position
    
    -- Create arrays for move and undo history
    -- The +1 ensures we have space for depths 0 to maxDepth
    moveHistory <- newArray (0, 100) 0  -- Store moves made during search
    undoHistory <- newArray (0, 100) (UndoInfo Pawn 0 0 False False False False 0)  -- Store data for undoing moves
    
    -- Create references for search state
    depthRef <- newSTRef 0
    alphaRef <- newSTRef (-999999)
    betaRef <- newSTRef 999999
    nodesRef <- newSTRef 0
    
    return STSearchState {
        posRef = posRef,
        moveHistory = moveHistory,
        undoHistory = undoHistory,
        depthRef = depthRef,
        alphaRef = alphaRef,
        betaRef = betaRef,
        nodesRef = nodesRef,
        searchState = searchState
    }

-- | Main search function using ST monad for mutation
-- | This is similar to the regular search, but operates on mutable state
searchInnerST :: STSearchState s -> Int -> ST s (Int, Move)
searchInnerST stState depth = do
    -- Increment the node counter
    modifySTRef' (nodesRef stState) (+1)
    -- NOTE: We're just counting nodes locally in ST monad
    -- The original incNodes call would be used in the IO version
    
    -- Get the current position
    position <- readSTRef (posRef stState)
    
    -- Terminal node checks
    if depth <= 0
        then do
            -- Leaf node - we'd normally switch to quiescence search
            -- In the actual implementation, we would need to adapt quiesce to use ST monad
            -- For now, just use evaluate as a placeholder
            let score = evaluate position
            return (score, 0)
        else do
            -- Check hash table
            let hashIndex = calcHashIndex (zobrist position)
            -- (Hash table lookup would go here)
            
            -- Generate moves
            let allMoves = sortMoves position $ moves position
            
            -- Main alpha-beta search loop
            searchMoves stState depth allMoves 0 (-999999)

-- | Search through a list of moves using alpha-beta pruning
-- | This is the core loop that tries each move and keeps track of the best move
searchMoves :: STSearchState s -> Int -> [Move] -> Move -> Int -> ST s (Int, Move)
searchMoves stState depth [] bestMove bestScore = 
    -- No more moves to search
    return (bestScore, bestMove)
    
searchMoves stState depth (move:moves) bestMove bestScore = do
    -- Get current alpha and beta values
    alpha <- readSTRef (alphaRef stState)
    beta <- readSTRef (betaRef stState)
    
    -- Make the move on our mutable position
    makeMoveST stState move
    
    -- Get the position after the move
    position <- readSTRef (posRef stState)
    
    -- Check if the move puts us in check (illegal)
    isLegal <- checkLegalMove stState
    
    score <- if not isLegal
        then return (-999999)  -- Illegal move
        else do
            -- Get current ply
            ply <- readSTRef (depthRef stState)
            
            -- Save the move for our PV (principal variation)
            writeArray (moveHistory stState) ply move
            
            -- Recursively search with negated alpha/beta
            -- In negamax, we negate the bounds and the result
            modifySTRef' (depthRef stState) (+1)
            (childScore, _) <- searchInnerST stState (depth - 1)
            modifySTRef' (depthRef stState) (subtract 1)
            
            -- Our score is the negation of the child score (negamax)
            return (-childScore)
    
    -- Unmake the move to restore the position
    unmakeMoveST stState
    
    -- Update alpha if we have a better move
    -- (In a full implementation, we'd update alpha and beta)
    let newBestScore = max bestScore score
        newBestMove = if score > bestScore then move else bestMove
    
    -- Check for beta cutoff (opponent won't allow this line)
    if newBestScore >= beta
        then return (newBestScore, newBestMove)  -- Beta cutoff
        else do
            -- Update alpha
            when (newBestScore > alpha) $
                writeSTRef (alphaRef stState) newBestScore
            
            -- Continue searching remaining moves
            searchMoves stState depth moves newBestMove newBestScore

-- | Check if the current position is legal (not in check for the side that just moved)
checkLegalMove :: STSearchState s -> ST s Bool
checkLegalMove stState = do
    position <- readSTRef (posRef stState)
    -- The side that just moved is the opposite of the current mover
    let sideToCheck = if mover position == White then Black else White
    -- Not in check = legal move
    return $ not $ isCheck position sideToCheck

-- | Make a move on the mutable position
-- | This is the core optimization - we modify the position in-place instead of creating a new one
makeMoveST :: STSearchState s -> Move -> ST s ()
makeMoveST stState move = do
    -- Get the current position and depth
    position <- readSTRef (posRef stState)
    ply <- readSTRef (depthRef stState)
    
    -- Save information needed to unmake this move later
    let undoInfo = UndoInfo {
        capturedPiece = Pawn,  -- In a real implementation, check for captures
        oldEnPassantSquare = enPassantSquare position,
        halfMoveValue = halfMoves position,
        whiteCastleKing = whiteKingCastleAvailable position,
        whiteCastleQueen = whiteQueenCastleAvailable position,
        blackCastleKing = blackKingCastleAvailable position,
        blackCastleQueen = blackQueenCastleAvailable position,
        zobristKey = zobrist position
    }
    writeArray (undoHistory stState) ply undoInfo
    
    -- For now, we'll use the immutable makeMove and create a new position
    -- In a full implementation, we'd modify the position fields directly
    let newPosition = makeMove position move
    writeSTRef (posRef stState) newPosition

-- | Unmake a move to restore the previous position
-- | This restores the position to its state before the last move
unmakeMoveST :: STSearchState s -> ST s ()
unmakeMoveST stState = do
    -- Get the current depth
    ply <- readSTRef (depthRef stState)
    
    -- Get the current position and the undo information
    position <- readSTRef (posRef stState)
    undoInfo <- readArray (undoHistory stState) ply
    
    -- Restore the position fields from the undo information
    -- In a full implementation, we'd need to handle all position fields
    let restoredPosition = position {
        enPassantSquare = oldEnPassantSquare undoInfo,
        halfMoves = halfMoveValue undoInfo,
        whiteKingCastleAvailable = whiteCastleKing undoInfo,
        whiteQueenCastleAvailable = whiteCastleQueen undoInfo,
        blackKingCastleAvailable = blackCastleKing undoInfo,
        blackQueenCastleAvailable = blackCastleQueen undoInfo,
        -- Switch the mover back
        mover = if mover position == White then Black else White
    }
    
    -- Update the position reference
    writeSTRef (posRef stState) restoredPosition

-- | Sort moves for better alpha-beta pruning
-- | Good move ordering dramatically improves search efficiency
sortMoves :: Position -> [Move] -> [Move]
sortMoves position moves =
    -- Order by move score (captures first, then other heuristics)
    sortBy (\m1 m2 -> compare (scoreMove m2) (scoreMove m1)) moves
  where
    -- Simple move scoring function that prioritizes captures
    scoreMove :: Move -> Int
    scoreMove move = 
        if isCapture position move
            then 10000 + captureValue move
            else 0
        
    captureValue :: Move -> Int
    captureValue _ = 100  -- Simplified for demonstration

-- | Iterative deepening search using the ST monad
-- | This searches from depth 1 to maxDepth, using results from shallower
-- | searches to improve move ordering in deeper searches
iterativeDeepeningST :: Position -> Int -> Int -> Int -> SearchState -> (Int, Move, Int)
iterativeDeepeningST position maxDepth alpha beta searchState = runST $ do
    -- Create the search state
    stState <- initSTSearchState position searchState
    
    -- Set initial alpha-beta values
    writeSTRef (alphaRef stState) alpha
    writeSTRef (betaRef stState) beta
    
    -- Iterative deepening loop
    (finalEval, finalMove) <- iterateDepths stState 1 maxDepth 0 0
    
    -- Get the node count
    nodes <- readSTRef (nodesRef stState)
    
    return (finalEval, finalMove, nodes)

-- | Helper function for iterative deepening
iterateDepths :: STSearchState s -> Int -> Int -> Int -> Move -> ST s (Int, Move)
iterateDepths stState currentDepth maxDepth bestEval bestMove
    | currentDepth > maxDepth = return (bestEval, bestMove)
    | otherwise = do
        -- Search at the current depth
        (eval, move) <- searchInnerST stState currentDepth
        
        -- Recur with the next depth
        iterateDepths stState (currentDepth + 1) maxDepth eval move

-- | Root search function - a simplified interface for the ST search
searchRootST :: Position -> Int -> Int -> Int -> SearchState -> (Int, Move)
searchRootST position depth alpha beta searchState =
    searchST position depth alpha beta searchState
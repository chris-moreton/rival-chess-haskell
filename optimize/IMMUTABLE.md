Let me explain the core issue with immutable data structures in a chess engine and how to improve performance with mutable alternatives.

Immutable Position Problem in Search

When using immutable data structures like your Position record, every time you make a move, you create an entirely new Position value. In a chess search that might examine millions of positions, this creates significant overhead:

1. Memory allocation for each new position
2. Garbage collection pressure as positions are discarded
3. Cache inefficiency from constant object creation

Solution 1: Mutable References with ST Monad

The ST (State Thread) monad in Haskell allows you to use mutable state in a controlled, safe way. Instead of creating new positions, you can modify a single position in-place as you search.

Here's how you might implement it:

import Control.Monad.ST
import Data.STRef

-- A function that performs search using mutable position
searchST :: Position -> Int -> (Int, Move)
searchST initialPos depth = runST $ do
-- Create a mutable reference to the position
posRef <- newSTRef initialPos

      -- Now we can modify this position in-place
      let searchMoves alpha beta = do
              -- Read the current position
              pos <- readSTRef posRef

              -- Generate moves
              let moves = generateMoves pos

              -- For each move
              -- (simplified for illustration)
              foldM (\(bestScore, bestMove) move -> do
                  -- Make the move by modifying the position in-place
                  makeMoveST posRef move

                  -- Recursively search
                  score <- negamax (depth-1) (-beta) (-alpha)

                  -- Unmake the move to restore the position
                  unmakeMoveST posRef

                  -- Return the best score so far
                  return $ if -score > bestScore
                          then (-score, move)
                          else (bestScore, bestMove)
              ) (-999999, 0) moves

      -- Run the search
      searchMoves (-999999) 999999

The key components:
- STRef Position is a mutable reference to a position
- makeMoveST and unmakeMoveST modify the position in place
- We avoid creating new position records for each move

Solution 2: Implement Make/Unmake Move Pair

To efficiently implement this approach, you need to create two functions:

1. makeMoveST: Apply a move to the position, modifying it in place
2. unmakeMoveST: Restore the position to its state before the move

makeMoveST :: STRef s Position -> Move -> ST s ()
makeMoveST posRef move = do
pos <- readSTRef posRef

      -- 1. Store any irreversible state for unmake
      -- For example, save the captured piece, en passant state, etc.
      -- This is typically done in a stack or array that we can pop from

      -- 2. Update bitboards in place
      -- For example, if a pawn moves from e2 to e4:
      let fromSq = getFromSquare move
          toSq = getToSquare move
          newWhitePawnBB = clearBit (whitePawnBitboard pos) fromSq .|. bit toSq

      -- 3. Write the modified position back to the reference
      writeSTRef posRef $ pos {
          whitePawnBitboard = newWhitePawnBB,
          -- Update other fields as needed
          mover = switchSide (mover pos)
      }

unmakeMoveST :: STRef s Position -> ST s ()
unmakeMoveST posRef = do
-- 1. Pop stored state from our stack/array
-- 2. Restore position fields to their previous values
-- 3. Write the restored position back to the reference

Implementation Challenges

1. Saving State: You need an efficient way to store only what changed to restore later
2. Move Type: You may need to adapt your move representation to include info needed for unmake
3. Zobrist Keys: If you use incremental Zobrist hashing, you need to efficiently update keys

Real-World Example Structure

In a complete implementation, you might have:

data SearchState s = SearchState {
position :: STRef s Position,
moveHistory :: STRef s (Array Int Move),  -- Moves we've made
stateHistory :: STRef s (Array Int UndoInfo),  -- Data for unmake
historyIndex :: STRef s Int  -- Current depth in search
}

data UndoInfo = UndoInfo {
capturedPiece :: Maybe Piece,
enPassantSquare :: Square,
castlingRights :: CastlingRights,
halfMoveClock :: Int,
zobristKey :: Zobrist
}

This approach dramatically reduces allocations during search and can easily provide 2-3x speedup for deep searches.